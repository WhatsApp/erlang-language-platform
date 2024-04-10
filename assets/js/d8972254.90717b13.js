"use strict";(self.webpackChunkstaticdocs_starter=self.webpackChunkstaticdocs_starter||[]).push([[3052],{15680:(e,t,r)=>{r.r(t),r.d(t,{MDXContext:()=>p,MDXProvider:()=>d,mdx:()=>h,useMDXComponents:()=>u,withMDXComponents:()=>l});var o=r(96540);function n(e,t,r){return t in e?Object.defineProperty(e,t,{value:r,enumerable:!0,configurable:!0,writable:!0}):e[t]=r,e}function a(){return a=Object.assign||function(e){for(var t=1;t<arguments.length;t++){var r=arguments[t];for(var o in r)Object.prototype.hasOwnProperty.call(r,o)&&(e[o]=r[o])}return e},a.apply(this,arguments)}function i(e,t){var r=Object.keys(e);if(Object.getOwnPropertySymbols){var o=Object.getOwnPropertySymbols(e);t&&(o=o.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),r.push.apply(r,o)}return r}function c(e){for(var t=1;t<arguments.length;t++){var r=null!=arguments[t]?arguments[t]:{};t%2?i(Object(r),!0).forEach((function(t){n(e,t,r[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(r)):i(Object(r)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(r,t))}))}return e}function s(e,t){if(null==e)return{};var r,o,n=function(e,t){if(null==e)return{};var r,o,n={},a=Object.keys(e);for(o=0;o<a.length;o++)r=a[o],t.indexOf(r)>=0||(n[r]=e[r]);return n}(e,t);if(Object.getOwnPropertySymbols){var a=Object.getOwnPropertySymbols(e);for(o=0;o<a.length;o++)r=a[o],t.indexOf(r)>=0||Object.prototype.propertyIsEnumerable.call(e,r)&&(n[r]=e[r])}return n}var p=o.createContext({}),l=function(e){return function(t){var r=u(t.components);return o.createElement(e,a({},t,{components:r}))}},u=function(e){var t=o.useContext(p),r=t;return e&&(r="function"==typeof e?e(t):c(c({},t),e)),r},d=function(e){var t=u(e.components);return o.createElement(p.Provider,{value:t},e.children)},f="mdxType",g={inlineCode:"code",wrapper:function(e){var t=e.children;return o.createElement(o.Fragment,{},t)}},m=o.forwardRef((function(e,t){var r=e.components,n=e.mdxType,a=e.originalType,i=e.parentName,p=s(e,["components","mdxType","originalType","parentName"]),l=u(r),d=n,f=l["".concat(i,".").concat(d)]||l[d]||g[d]||a;return r?o.createElement(f,c(c({ref:t},p),{},{components:r})):o.createElement(f,c({ref:t},p))}));function h(e,t){var r=arguments,n=t&&t.mdxType;if("string"==typeof e||n){var a=r.length,i=new Array(a);i[0]=m;var c={};for(var s in t)hasOwnProperty.call(t,s)&&(c[s]=t[s]);c.originalType=e,c[f]="string"==typeof e?e:n,i[1]=c;for(var p=2;p<a;p++)i[p]=r[p];return o.createElement.apply(null,i)}return o.createElement.apply(null,r)}m.displayName="MDXCreateElement"},24005:(e,t,r)=>{r.r(t),r.d(t,{assets:()=>s,contentTitle:()=>i,default:()=>d,frontMatter:()=>a,metadata:()=>c,toc:()=>p});var o=r(58168),n=(r(96540),r(15680));const a={sidebar_position:1},i="Configure Your Project",c={unversionedId:"get-started/configure-project/configure-project",id:"get-started/configure-project/configure-project",title:"Configure Your Project",description:"When used as a language server - via the elp server command or via a text editor extension -, ELP needs to be aware of the structure of the project. This is essential for ELP to correctly identify dependencies, header files and the alike. Failing the discovery phase results in a degraded language server, where features such as auto-completion or go-to-definition do not work as expected.",source:"@site/docs/get-started/configure-project/configure-project.md",sourceDirName:"get-started/configure-project",slug:"/get-started/configure-project/",permalink:"/erlang-language-platform/docs/get-started/configure-project/",draft:!1,tags:[],version:"current",sidebarPosition:1,frontMatter:{sidebar_position:1},sidebar:"tutorialSidebar",previous:{title:"VS Code",permalink:"/erlang-language-platform/docs/get-started/editors/vscode"},next:{title:"Buck2",permalink:"/erlang-language-platform/docs/get-started/configure-project/buck2"}},s={},p=[],l={toc:p},u="wrapper";function d(e){let{components:t,...r}=e;return(0,n.mdx)(u,(0,o.A)({},l,r,{components:t,mdxType:"MDXLayout"}),(0,n.mdx)("h1",{id:"configure-your-project"},"Configure Your Project"),(0,n.mdx)("p",null,"When used as a language server - via the ",(0,n.mdx)("a",{parentName:"p",href:"/erlang-language-platform/docs/get-started/cli#elp-server"},"elp server")," command or via a text editor extension -, ELP needs to be aware of the structure of the project. This is essential for ELP to correctly identify dependencies, header files and the alike. Failing the discovery phase results in a degraded language server, where features such as ",(0,n.mdx)("em",{parentName:"p"},"auto-completion")," or ",(0,n.mdx)("em",{parentName:"p"},"go-to-definition")," do not work as expected."),(0,n.mdx)("p",null,"For ",(0,n.mdx)("inlineCode",{parentName:"p"},"rebar3")," projects (i.e. when a ",(0,n.mdx)("inlineCode",{parentName:"p"},"rebar.config")," or ",(0,n.mdx)("inlineCode",{parentName:"p"},"rebar.config.script")," file is encountered), ELP attempts to automatically discover the structure of the project when you first open a file (this currently requires a dedicated ",(0,n.mdx)("a",{parentName:"p",href:"/erlang-language-platform/docs/get-started/configure-project/rebar3#install-the-rebar3-build-info-plugin"},"plugin"),"). It is also possible to explicitly create a configuration file, named ",(0,n.mdx)("a",{parentName:"p",href:"/erlang-language-platform/docs/get-started/configure-project/elp-toml"},".elp.toml")," in the root directory of a project, to provide ELP explicit information about the project structure."),(0,n.mdx)("p",null,"ELP can also load the project structure via the ",(0,n.mdx)("a",{parentName:"p",href:"/erlang-language-platform/docs/get-started/configure-project/elp-toml#buck"},"Buck2")," build system or, for ",(0,n.mdx)("a",{parentName:"p",href:"/erlang-language-platform/docs/get-started/configure-project/custom-project"},"custom projects"),", via a dedicated ",(0,n.mdx)("a",{parentName:"p",href:"/erlang-language-platform/docs/get-started/configure-project/custom-project#the-build_infojson-format"},"build_info.json")," file."))}d.isMDXComponent=!0}}]);