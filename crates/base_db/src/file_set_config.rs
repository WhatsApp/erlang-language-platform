/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! `vfs::file_set::FileSetConfig`, whose `partition` takes a `vfs::Vfs`,
//! ported to partition a [`FileMap`] with the same semantics.

use fst::IntoStreamer;
use fst::Streamer;
use vfs::VfsPath;
use vfs::file_set::FileSet;

use crate::FileMap;

/// Path prefixes that partition the existing files of a [`FileMap`] into
/// [`FileSet`]s. A file goes to the set with the longest root that is a byte
/// prefix of its parent directory, so root `/a/b` also claims `/a/bc/x.erl`;
/// files no root claims go to an extra, last set.
#[derive(Debug)]
pub struct FileSetConfig {
    n_file_sets: usize,
    /// Encoded root paths, mapped to the index of their set.
    map: fst::Map<Vec<u8>>,
}

impl Default for FileSetConfig {
    fn default() -> Self {
        FileSetConfig::builder().build()
    }
}

impl FileSetConfig {
    pub fn builder() -> FileSetConfigBuilder {
        FileSetConfigBuilder::default()
    }

    pub fn partition(&self, file_map: &FileMap) -> Vec<FileSet> {
        let mut scratch_space = Vec::new();
        let mut res = vec![FileSet::default(); self.n_file_sets];
        for (file_id, path) in file_map.existing_files() {
            let root = self.classify(&path, &mut scratch_space);
            res[root].insert(file_id, path);
        }
        res
    }

    fn classify(&self, path: &VfsPath, scratch_space: &mut Vec<u8>) -> usize {
        let path = path.parent().unwrap_or_else(|| path.clone());

        scratch_space.clear();
        encode(&path, scratch_space);
        let automaton = PrefixOf::new(scratch_space.as_slice());
        let mut longest_prefix = self.n_file_sets - 1;
        let mut stream = self.map.search(automaton).into_stream();
        while let Some((_, v)) = stream.next() {
            longest_prefix = v as usize;
        }
        longest_prefix
    }
}

#[derive(Default)]
pub struct FileSetConfigBuilder {
    roots: Vec<Vec<VfsPath>>,
}

impl FileSetConfigBuilder {
    /// Adds a set of path prefixes. A prefix already in an earlier set stays
    /// there.
    pub fn add_file_set(&mut self, roots: Vec<VfsPath>) {
        self.roots.push(roots);
    }

    pub fn build(self) -> FileSetConfig {
        let n_file_sets = self.roots.len() + 1;
        let map = {
            let mut entries = Vec::new();
            for (i, paths) in self.roots.into_iter().enumerate() {
                for p in paths {
                    let mut buf = Vec::new();
                    encode(&p, &mut buf);
                    entries.push((buf, i as u64));
                }
            }
            entries.sort();
            entries.dedup_by(|(a, _), (b, _)| a == b);
            fst::Map::from_iter(entries).expect("sorted and deduplicated keys should build a map")
        };
        FileSetConfig { n_file_sets, map }
    }
}

/// Matches if `prefix_of` is a prefix of the given data.
struct PrefixOf<'a> {
    prefix_of: &'a [u8],
}

impl<'a> PrefixOf<'a> {
    fn new(prefix_of: &'a [u8]) -> Self {
        Self { prefix_of }
    }
}

impl fst::Automaton for PrefixOf<'_> {
    type State = usize;
    fn start(&self) -> usize {
        0
    }
    fn is_match(&self, &state: &usize) -> bool {
        state != !0
    }
    fn can_match(&self, &state: &usize) -> bool {
        state != !0
    }
    fn accept(&self, &state: &usize, byte: u8) -> usize {
        if self.prefix_of.get(state) == Some(&byte) {
            state + 1
        } else {
            !0
        }
    }
}

/// `VfsPath::encode`, which `vfs` keeps private.
fn encode(path: &VfsPath, buf: &mut Vec<u8>) {
    let Some(path) = path.as_path() else {
        buf.push(1);
        buf.extend(path.to_string().as_bytes());
        return;
    };
    buf.push(0);
    #[cfg(windows)]
    {
        use windows_paths::Encode;
        let path: &std::path::Path = path.as_ref();
        let components = path.components();
        let mut add_sep = false;
        for component in components {
            if add_sep {
                windows_paths::SEP.encode(buf);
            }
            let len_before = buf.len();
            match component {
                std::path::Component::Prefix(prefix) => {
                    // kind() returns a normalized and comparable path prefix.
                    prefix.kind().encode(buf);
                }
                std::path::Component::RootDir => {
                    if !add_sep {
                        component.as_os_str().encode(buf);
                    }
                }
                _ => component.as_os_str().encode(buf),
            }

            // some components may be encoded empty
            add_sep = len_before != buf.len();
        }
    }
    #[cfg(unix)]
    {
        use std::os::unix::ffi::OsStrExt;
        buf.extend(path.as_os_str().as_bytes());
    }
    #[cfg(not(any(windows, unix)))]
    {
        buf.extend(path.as_os_str().to_string_lossy().as_bytes());
    }
}

#[cfg(windows)]
mod windows_paths {
    pub(crate) trait Encode {
        fn encode(&self, buf: &mut Vec<u8>);
    }

    impl Encode for std::ffi::OsStr {
        fn encode(&self, buf: &mut Vec<u8>) {
            use std::os::windows::ffi::OsStrExt;
            for wchar in self.encode_wide() {
                buf.extend(wchar.to_le_bytes().iter().copied());
            }
        }
    }

    impl Encode for u8 {
        fn encode(&self, buf: &mut Vec<u8>) {
            let wide = *self as u16;
            buf.extend(wide.to_le_bytes().iter().copied())
        }
    }

    impl Encode for &str {
        fn encode(&self, buf: &mut Vec<u8>) {
            debug_assert!(self.is_ascii());
            for b in self.as_bytes() {
                b.encode(buf)
            }
        }
    }

    pub(crate) const SEP: &str = "\\";
    const VERBATIM: &str = "\\\\?\\";
    const UNC: &str = "UNC";
    const DEVICE: &str = "\\\\.\\";
    const COLON: &str = ":";

    impl Encode for std::path::Prefix<'_> {
        fn encode(&self, buf: &mut Vec<u8>) {
            match self {
                std::path::Prefix::Verbatim(c) => {
                    VERBATIM.encode(buf);
                    c.encode(buf);
                }
                std::path::Prefix::VerbatimUNC(server, share) => {
                    VERBATIM.encode(buf);
                    UNC.encode(buf);
                    SEP.encode(buf);
                    server.encode(buf);
                    SEP.encode(buf);
                    share.encode(buf);
                }
                std::path::Prefix::VerbatimDisk(d) => {
                    VERBATIM.encode(buf);
                    d.encode(buf);
                    COLON.encode(buf);
                }
                std::path::Prefix::DeviceNS(device) => {
                    DEVICE.encode(buf);
                    device.encode(buf);
                }
                std::path::Prefix::UNC(server, share) => {
                    SEP.encode(buf);
                    SEP.encode(buf);
                    server.encode(buf);
                    SEP.encode(buf);
                    share.encode(buf);
                }
                std::path::Prefix::Disk(d) => {
                    d.encode(buf);
                    COLON.encode(buf);
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use vfs::Vfs;
    use vfs::VfsPath;

    use super::FileSetConfig;
    use crate::FileMap;

    /// The port must partition exactly as `vfs` does, including the byte prefix
    /// match of a file's directory against the roots.
    #[test]
    fn partition_agrees_with_vfs() {
        let roots: &[&[&str]] = &[
            &["/a/b"],
            &["/a/b/c", "/x"],
            // `/a/b` is already in the first set, where it stays.
            &["/a/b", "/d"],
            &["/v"],
        ];
        let files = [
            ("/a/b/f.erl", true),
            ("/a/bc/g.erl", true),
            ("/a/b/c/h.erl", true),
            ("/a/b/cd/i.erl", true),
            ("/a/b/c.erl", true),
            ("/xy/j.erl", true),
            ("/x.erl", true),
            ("/d/k.erl", true),
            ("/v/l.erl", true),
            ("/y/m.erl", true),
            ("/a/b/deleted.erl", false),
        ];

        let mut builder = FileSetConfig::builder();
        let mut vfs_builder = vfs::file_set::FileSetConfig::builder();
        for set in roots {
            let paths: Vec<VfsPath> = set
                .iter()
                .map(|root| VfsPath::new_real_path(root.to_string()))
                .collect();
            builder.add_file_set(paths.clone());
            vfs_builder.add_file_set(paths);
        }
        let file_map = FileMap::default();
        let mut vfs = Vfs::default();
        for (path, exists) in files {
            let path = VfsPath::new_real_path(path.to_string());
            file_map.set_exists(file_map.intern(path.clone()), exists);
            vfs.set_file_contents(path.clone(), Some(vec![]));
            if !exists {
                vfs.set_file_contents(path, None);
            }
        }
        // Never claimed by a root, which is a real path.
        let virtual_path = VfsPath::new_virtual_path("/v/n.erl".to_string());
        file_map.set_exists(file_map.intern(virtual_path.clone()), true);
        vfs.set_file_contents(virtual_path, Some(vec![]));

        let sets = builder.build().partition(&file_map);
        let vfs_sets = vfs_builder.build().partition(&vfs);

        let files_in = |sets: &[vfs::file_set::FileSet]| -> Vec<Vec<VfsPath>> {
            sets.iter()
                .map(|set| {
                    set.iter()
                        .map(|file_id| set.path_for_file(&file_id).unwrap().clone())
                        .collect()
                })
                .collect()
        };
        assert_eq!(
            files_in(&vfs_sets),
            files_in(&sets),
            "the port and vfs should put the same files, in the same order, into the same sets"
        );
        assert_eq!(vfs_sets, sets, "the port and vfs should agree on file ids");
        let expected_quirk = VfsPath::new_real_path("/a/bc/g.erl".to_string());
        assert!(
            sets[0].file_for_path(&expected_quirk).is_some(),
            "root /a/b should claim /a/bc/g.erl, which exercises the byte-prefix match"
        );
    }
}
