"use strict";(self.webpackChunkstaticdocs_starter=self.webpackChunkstaticdocs_starter||[]).push([[2647],{83831:(e,n,l)=>{l.r(n),l.d(n,{assets:()=>o,contentTitle:()=>s,default:()=>p,frontMatter:()=>a,metadata:()=>i,toc:()=>c});const i=JSON.parse('{"id":"get-started/cli","title":"The ELP CLI","description":"ELP can be used from the command line. Please ensure you read the install section to learn how to install it.","source":"@site/docs/get-started/cli.md","sourceDirName":"get-started","slug":"/get-started/cli","permalink":"/erlang-language-platform/docs/get-started/cli","draft":false,"unlisted":false,"tags":[],"version":"current","sidebarPosition":3,"frontMatter":{"sidebar_position":3},"sidebar":"tutorialSidebar","previous":{"title":"Install ELP","permalink":"/erlang-language-platform/docs/get-started/install"},"next":{"title":"Configure Your Editor","permalink":"/erlang-language-platform/docs/get-started/editors/"}}');var r=l(74848),t=l(28453);const a={sidebar_position:3},s="The ELP CLI",o={},c=[{value:"Verify <code>elp</code> is correctly installed",id:"verify-elp-is-correctly-installed",level:2},{value:"Getting Help",id:"getting-help",level:2},{value:"<code>elp server</code>",id:"elp-server",level:2},{value:"<code>elp eqwalize</code>",id:"elp-eqwalize",level:2},{value:"<code>elp eqwalize-all</code>",id:"elp-eqwalize-all",level:2}];function d(e){const n={a:"a",admonition:"admonition",code:"code",em:"em",h1:"h1",h2:"h2",header:"header",p:"p",pre:"pre",...(0,t.R)(),...e.components};return(0,r.jsxs)(r.Fragment,{children:[(0,r.jsx)(n.header,{children:(0,r.jsx)(n.h1,{id:"the-elp-cli",children:"The ELP CLI"})}),"\n",(0,r.jsxs)(n.p,{children:["ELP can be used from the command line. Please ensure you read the ",(0,r.jsx)(n.a,{href:"/erlang-language-platform/docs/get-started/install",children:"install"})," section to learn how to install it."]}),"\n",(0,r.jsxs)(n.h2,{id:"verify-elp-is-correctly-installed",children:["Verify ",(0,r.jsx)(n.code,{children:"elp"})," is correctly installed"]}),"\n",(0,r.jsx)(n.admonition,{type:"tip",children:(0,r.jsxs)(n.p,{children:['On Mac you may get a warning, saying "elp cannot be opened because the developer cannot be verified". To solve this, go to ',(0,r.jsx)(n.code,{children:"Preferences -> Security and Privacy -> General"})," and add an exception for ",(0,r.jsx)(n.code,{children:"elp"}),". Alternatively, you can build the project from source (see below)"]})}),"\n",(0,r.jsx)(n.p,{children:"Open a terminal and run:"}),"\n",(0,r.jsx)(n.pre,{children:(0,r.jsx)(n.code,{children:"$ elp version\n"})}),"\n",(0,r.jsx)(n.p,{children:"You should see something like:"}),"\n",(0,r.jsx)(n.pre,{children:(0,r.jsx)(n.code,{children:"elp 1.1.0+build-2024-01-18\n"})}),"\n",(0,r.jsx)(n.p,{children:"If that's the case, you're ready to roll!"}),"\n",(0,r.jsx)(n.h2,{id:"getting-help",children:"Getting Help"}),"\n",(0,r.jsxs)(n.p,{children:["All ",(0,r.jsx)(n.code,{children:"elp"})," commands are available through the help:"]}),"\n",(0,r.jsx)(n.pre,{children:(0,r.jsx)(n.code,{children:"$ elp -h\n\nUsage: [--log-file LOG_FILE] [--no-log-buffering] [COMMAND ...]\n\nAvailable options:\n        --log-file <LOG_FILE>\n        --no-log-buffering\n    -h, --help                 Prints help information\n\nAvailable commands:\n    eqwalize              Eqwalize specified module\n    eqwalize-all          Eqwalize all opted-in modules in a project\n    eqwalize-app          Eqwalize all opted-in modules in specified application\n    eqwalize-target       Eqwalize all opted-in modules in specified buck target\n    lint                  Parse files in project and emit diagnostics, optionally apply fixes.\n    server                Run lsp server\n    generate-completions  Generate shell completions\n    parse-all             Dump ast for all files in a project for specified rebar.config file\n    parse-elp             Tree-sitter parse all files in a project for specified rebar.config file\n    build-info            Generate build info file\n    version               Print version\n    shell                 Starts an interactive ELP shell\n    eqwalize-stats        Return statistics about code quality for eqWAlizer\n    explain               Explain a diagnostic code\n    project-info          Generate project info file\n    glean                 Glean indexer\n"})}),"\n",(0,r.jsx)(n.h2,{id:"elp-server",children:(0,r.jsx)(n.code,{children:"elp server"})}),"\n",(0,r.jsx)(n.p,{children:"Start a LSP server. The command does not return."}),"\n",(0,r.jsx)(n.pre,{children:(0,r.jsx)(n.code,{children:"$ elp server\n"})}),"\n",(0,r.jsx)(n.h2,{id:"elp-eqwalize",children:(0,r.jsx)(n.code,{children:"elp eqwalize"})}),"\n",(0,r.jsxs)(n.p,{children:["Run the ",(0,r.jsx)(n.em,{children:"eqWAlizer"})," typechecker against an Erlang module."]}),"\n",(0,r.jsx)(n.pre,{children:(0,r.jsx)(n.code,{children:"$ elp eqwalize <module>\n"})}),"\n",(0,r.jsx)(n.h2,{id:"elp-eqwalize-all",children:(0,r.jsx)(n.code,{children:"elp eqwalize-all"})}),"\n",(0,r.jsxs)(n.p,{children:["Run the ",(0,r.jsx)(n.em,{children:"eqWAlizer"})," typechecker against all ",(0,r.jsx)(n.em,{children:"src"})," modules in a project."]}),"\n",(0,r.jsx)(n.pre,{children:(0,r.jsx)(n.code,{children:"$ elp eqwalize-all\n"})})]})}function p(e={}){const{wrapper:n}={...(0,t.R)(),...e.components};return n?(0,r.jsx)(n,{...e,children:(0,r.jsx)(d,{...e})}):d(e)}},28453:(e,n,l)=>{l.d(n,{R:()=>a,x:()=>s});var i=l(96540);const r={},t=i.createContext(r);function a(e){const n=i.useContext(t);return i.useMemo((function(){return"function"==typeof e?e(n):{...n,...e}}),[n,e])}function s(e){let n;return n=e.disableParentContext?"function"==typeof e.components?e.components(r):e.components||r:a(e.components),i.createElement(t.Provider,{value:n},e.children)}}}]);