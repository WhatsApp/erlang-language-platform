"use strict";(self.webpackChunkstaticdocs_starter=self.webpackChunkstaticdocs_starter||[]).push([[4985],{15680:(e,r,n)=>{n.r(r),n.d(r,{MDXContext:()=>p,MDXProvider:()=>d,mdx:()=>b,useMDXComponents:()=>s,withMDXComponents:()=>u});var t=n(96540);function o(e,r,n){return r in e?Object.defineProperty(e,r,{value:n,enumerable:!0,configurable:!0,writable:!0}):e[r]=n,e}function a(){return a=Object.assign||function(e){for(var r=1;r<arguments.length;r++){var n=arguments[r];for(var t in n)Object.prototype.hasOwnProperty.call(n,t)&&(e[t]=n[t])}return e},a.apply(this,arguments)}function i(e,r){var n=Object.keys(e);if(Object.getOwnPropertySymbols){var t=Object.getOwnPropertySymbols(e);r&&(t=t.filter((function(r){return Object.getOwnPropertyDescriptor(e,r).enumerable}))),n.push.apply(n,t)}return n}function l(e){for(var r=1;r<arguments.length;r++){var n=null!=arguments[r]?arguments[r]:{};r%2?i(Object(n),!0).forEach((function(r){o(e,r,n[r])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(n)):i(Object(n)).forEach((function(r){Object.defineProperty(e,r,Object.getOwnPropertyDescriptor(n,r))}))}return e}function c(e,r){if(null==e)return{};var n,t,o=function(e,r){if(null==e)return{};var n,t,o={},a=Object.keys(e);for(t=0;t<a.length;t++)n=a[t],r.indexOf(n)>=0||(o[n]=e[n]);return o}(e,r);if(Object.getOwnPropertySymbols){var a=Object.getOwnPropertySymbols(e);for(t=0;t<a.length;t++)n=a[t],r.indexOf(n)>=0||Object.prototype.propertyIsEnumerable.call(e,n)&&(o[n]=e[n])}return o}var p=t.createContext({}),u=function(e){return function(r){var n=s(r.components);return t.createElement(e,a({},r,{components:n}))}},s=function(e){var r=t.useContext(p),n=r;return e&&(n="function"==typeof e?e(r):l(l({},r),e)),n},d=function(e){var r=s(e.components);return t.createElement(p.Provider,{value:r},e.children)},f="mdxType",m={inlineCode:"code",wrapper:function(e){var r=e.children;return t.createElement(t.Fragment,{},r)}},g=t.forwardRef((function(e,r){var n=e.components,o=e.mdxType,a=e.originalType,i=e.parentName,p=c(e,["components","mdxType","originalType","parentName"]),u=s(n),d=o,f=u["".concat(i,".").concat(d)]||u[d]||m[d]||a;return n?t.createElement(f,l(l({ref:r},p),{},{components:n})):t.createElement(f,l({ref:r},p))}));function b(e,r){var n=arguments,o=r&&r.mdxType;if("string"==typeof e||o){var a=n.length,i=new Array(a);i[0]=g;var l={};for(var c in r)hasOwnProperty.call(r,c)&&(l[c]=r[c]);l.originalType=e,l[f]="string"==typeof e?e:o,i[1]=l;for(var p=2;p<a;p++)i[p]=n[p];return t.createElement.apply(null,i)}return t.createElement.apply(null,n)}g.displayName="MDXCreateElement"},67484:(e,r,n)=>{n.r(r),n.d(r,{assets:()=>c,contentTitle:()=>i,default:()=>d,frontMatter:()=>a,metadata:()=>l,toc:()=>p});var t=n(58168),o=(n(96540),n(15680));const a={sidebar_position:1},i="L0003 - Unknown Application",l={unversionedId:"erlang-error-index/l/L0003",id:"erlang-error-index/l/L0003",title:"L0003 - Unknown Application",description:"Error",source:"@site/docs/erlang-error-index/l/L0003.md",sourceDirName:"erlang-error-index/l",slug:"/erlang-error-index/l/L0003",permalink:"/erlang-language-platform/docs/erlang-error-index/l/L0003",draft:!1,tags:[],version:"current",sidebarPosition:1,frontMatter:{sidebar_position:1},sidebar:"tutorialSidebar",previous:{title:"About",permalink:"/erlang-language-platform/docs/erlang-error-index/l/about"},next:{title:"L1201 - Undefined Module",permalink:"/erlang-language-platform/docs/erlang-error-index/l/L1201"}},c={},p=[{value:"Error",id:"error",level:2},{value:"Explanation",id:"explanation",level:2}],u={toc:p},s="wrapper";function d(e){let{components:r,...n}=e;return(0,o.mdx)(s,(0,t.A)({},u,n,{components:r,mdxType:"MDXLayout"}),(0,o.mdx)("h1",{id:"l0003---unknown-application"},"L0003 - Unknown Application"),(0,o.mdx)("h2",{id:"error"},"Error"),(0,o.mdx)("pre",null,(0,o.mdx)("code",{parentName:"pre",className:"language-erlang"},"%% ^ Error: Unknown application elp(L0003) [Ln 1, Col 1]\n")),(0,o.mdx)("h2",{id:"explanation"},"Explanation"),(0,o.mdx)("p",null,"This occurs when ELP tries to invoke the OTP Erlang Compiler on a\nmodule, but cannot find application data for it in the ELP project\nmodel."),(0,o.mdx)("p",null,"This normally means a misconfiguration of the build information for\nthe project, or in rare cases a bug in ELP."),(0,o.mdx)("p",null,"If the build information looks valid, please file a bug report."))}d.isMDXComponent=!0}}]);