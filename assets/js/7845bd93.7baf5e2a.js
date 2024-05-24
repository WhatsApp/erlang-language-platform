"use strict";(self.webpackChunkstaticdocs_starter=self.webpackChunkstaticdocs_starter||[]).push([[525],{15680:(e,t,n)=>{n.r(t),n.d(t,{MDXContext:()=>p,MDXProvider:()=>c,mdx:()=>v,useMDXComponents:()=>d,withMDXComponents:()=>m});var r=n(96540);function o(e,t,n){return t in e?Object.defineProperty(e,t,{value:n,enumerable:!0,configurable:!0,writable:!0}):e[t]=n,e}function a(){return a=Object.assign||function(e){for(var t=1;t<arguments.length;t++){var n=arguments[t];for(var r in n)Object.prototype.hasOwnProperty.call(n,r)&&(e[r]=n[r])}return e},a.apply(this,arguments)}function i(e,t){var n=Object.keys(e);if(Object.getOwnPropertySymbols){var r=Object.getOwnPropertySymbols(e);t&&(r=r.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),n.push.apply(n,r)}return n}function s(e){for(var t=1;t<arguments.length;t++){var n=null!=arguments[t]?arguments[t]:{};t%2?i(Object(n),!0).forEach((function(t){o(e,t,n[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(n)):i(Object(n)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(n,t))}))}return e}function l(e,t){if(null==e)return{};var n,r,o=function(e,t){if(null==e)return{};var n,r,o={},a=Object.keys(e);for(r=0;r<a.length;r++)n=a[r],t.indexOf(n)>=0||(o[n]=e[n]);return o}(e,t);if(Object.getOwnPropertySymbols){var a=Object.getOwnPropertySymbols(e);for(r=0;r<a.length;r++)n=a[r],t.indexOf(n)>=0||Object.prototype.propertyIsEnumerable.call(e,n)&&(o[n]=e[n])}return o}var p=r.createContext({}),m=function(e){return function(t){var n=d(t.components);return r.createElement(e,a({},t,{components:n}))}},d=function(e){var t=r.useContext(p),n=t;return e&&(n="function"==typeof e?e(t):s(s({},t),e)),n},c=function(e){var t=d(e.components);return r.createElement(p.Provider,{value:t},e.children)},u="mdxType",g={inlineCode:"code",wrapper:function(e){var t=e.children;return r.createElement(r.Fragment,{},t)}},f=r.forwardRef((function(e,t){var n=e.components,o=e.mdxType,a=e.originalType,i=e.parentName,p=l(e,["components","mdxType","originalType","parentName"]),m=d(n),c=o,u=m["".concat(i,".").concat(c)]||m[c]||g[c]||a;return n?r.createElement(u,s(s({ref:t},p),{},{components:n})):r.createElement(u,s({ref:t},p))}));function v(e,t){var n=arguments,o=t&&t.mdxType;if("string"==typeof e||o){var a=n.length,i=new Array(a);i[0]=f;var s={};for(var l in t)hasOwnProperty.call(t,l)&&(s[l]=t[l]);s.originalType=e,s[u]="string"==typeof e?e:o,i[1]=s;for(var p=2;p<a;p++)i[p]=n[p];return r.createElement.apply(null,i)}return r.createElement.apply(null,n)}f.displayName="MDXCreateElement"},91304:(e,t,n)=>{n.r(t),n.d(t,{assets:()=>l,contentTitle:()=>i,default:()=>c,frontMatter:()=>a,metadata:()=>s,toc:()=>p});var r=n(58168),o=(n(96540),n(15680));const a={sidebar_position:4},i="Neovim",s={unversionedId:"get-started/editors/neovim",id:"get-started/editors/neovim",title:"Neovim",description:"screenshot",source:"@site/docs/get-started/editors/neovim.md",sourceDirName:"get-started/editors",slug:"/get-started/editors/neovim",permalink:"/erlang-language-platform/docs/get-started/editors/neovim",draft:!1,tags:[],version:"current",sidebarPosition:4,frontMatter:{sidebar_position:4},sidebar:"tutorialSidebar",previous:{title:"VS Code",permalink:"/erlang-language-platform/docs/get-started/editors/vscode"},next:{title:"Configure Your Project",permalink:"/erlang-language-platform/docs/get-started/configure-project/"}},l={},p=[{value:"Updating ELP",id:"updating-elp",level:2},{value:"Troubleshooting",id:"troubleshooting",level:2}],m={toc:p},d="wrapper";function c(e){let{components:t,...a}=e;return(0,o.mdx)(d,(0,r.A)({},m,a,{components:t,mdxType:"MDXLayout"}),(0,o.mdx)("h1",{id:"neovim"},"Neovim"),(0,o.mdx)("p",null,(0,o.mdx)("img",{alt:"screenshot",src:n(64383).A,width:"1899",height:"902"})),(0,o.mdx)("p",null,"The ELP project can be used as a ",(0,o.mdx)("a",{parentName:"p",href:"https://microsoft.github.io/language-server-protocol/overviews/lsp/overview/"},"language server")," in Neovim."),(0,o.mdx)("p",null,"The easiest way to install the ELP server is to use ",(0,o.mdx)("a",{parentName:"p",href:"https://github.com/williamboman/mason.nvim"},"mason.nvim"),", a package manager\nfocused on language servers, linters and similar tools. If you use Neovim for programming, you likely already have ",(0,o.mdx)("inlineCode",{parentName:"p"},"mason.nvim"),' installed. It is also included in all the most popular "batteries included" configurations/distributions of Neovim such ',(0,o.mdx)("a",{parentName:"p",href:"https://github.com/nvim-lua/kickstart.nvim"},"kickstart.nvim"),", ",(0,o.mdx)("a",{parentName:"p",href:"https://github.com/LazyVim/LazyVim"},"LazyVim"),", ",(0,o.mdx)("a",{parentName:"p",href:"https://nvchad.com/"},"NVChad")," and others."),(0,o.mdx)("p",null,"With ",(0,o.mdx)("inlineCode",{parentName:"p"},"mason.nvim")," installed, you can install the ELP server by running the following command:"),(0,o.mdx)("pre",null,(0,o.mdx)("code",{parentName:"pre"},":MasonInstall elp\n")),(0,o.mdx)("p",null,"from within Neovim."),(0,o.mdx)("p",null,"Alternatively you can run ",(0,o.mdx)("inlineCode",{parentName:"p"},":Mason")," and browse the list of available packages, then press ",(0,o.mdx)("inlineCode",{parentName:"p"},"i")," when ",(0,o.mdx)("inlineCode",{parentName:"p"},"elp")," is highlighted."),(0,o.mdx)("h2",{id:"updating-elp"},"Updating ELP"),(0,o.mdx)("p",null,"You can update all packages managed by ",(0,o.mdx)("inlineCode",{parentName:"p"},"mason.nvim")," by running ",(0,o.mdx)("inlineCode",{parentName:"p"},":Mason")," and pressing ",(0,o.mdx)("inlineCode",{parentName:"p"},"U"),"."),(0,o.mdx)("h2",{id:"troubleshooting"},"Troubleshooting"),(0,o.mdx)("p",null,(0,o.mdx)("inlineCode",{parentName:"p"},":LspInfo")," shows the current status of the LSP client."),(0,o.mdx)("p",null,(0,o.mdx)("inlineCode",{parentName:"p"},":LspLog")," opens the log file for the LSP client, which can be useful for debugging."))}c.isMDXComponent=!0},64383:(e,t,n)=>{n.d(t,{A:()=>r});const r=n.p+"assets/images/neovim-d9675444d28e18ce6a03fe38db0cd38d.png"}}]);