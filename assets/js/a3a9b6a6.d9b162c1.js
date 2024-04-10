"use strict";(self.webpackChunkstaticdocs_starter=self.webpackChunkstaticdocs_starter||[]).push([[7032],{15680:(e,r,t)=>{t.r(r),t.d(r,{MDXContext:()=>c,MDXProvider:()=>d,mdx:()=>y,useMDXComponents:()=>u,withMDXComponents:()=>s});var n=t(96540);function o(e,r,t){return r in e?Object.defineProperty(e,r,{value:t,enumerable:!0,configurable:!0,writable:!0}):e[r]=t,e}function a(){return a=Object.assign||function(e){for(var r=1;r<arguments.length;r++){var t=arguments[r];for(var n in t)Object.prototype.hasOwnProperty.call(t,n)&&(e[n]=t[n])}return e},a.apply(this,arguments)}function i(e,r){var t=Object.keys(e);if(Object.getOwnPropertySymbols){var n=Object.getOwnPropertySymbols(e);r&&(n=n.filter((function(r){return Object.getOwnPropertyDescriptor(e,r).enumerable}))),t.push.apply(t,n)}return t}function l(e){for(var r=1;r<arguments.length;r++){var t=null!=arguments[r]?arguments[r]:{};r%2?i(Object(t),!0).forEach((function(r){o(e,r,t[r])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(t)):i(Object(t)).forEach((function(r){Object.defineProperty(e,r,Object.getOwnPropertyDescriptor(t,r))}))}return e}function p(e,r){if(null==e)return{};var t,n,o=function(e,r){if(null==e)return{};var t,n,o={},a=Object.keys(e);for(n=0;n<a.length;n++)t=a[n],r.indexOf(t)>=0||(o[t]=e[t]);return o}(e,r);if(Object.getOwnPropertySymbols){var a=Object.getOwnPropertySymbols(e);for(n=0;n<a.length;n++)t=a[n],r.indexOf(t)>=0||Object.prototype.propertyIsEnumerable.call(e,t)&&(o[t]=e[t])}return o}var c=n.createContext({}),s=function(e){return function(r){var t=u(r.components);return n.createElement(e,a({},r,{components:t}))}},u=function(e){var r=n.useContext(c),t=r;return e&&(t="function"==typeof e?e(r):l(l({},r),e)),t},d=function(e){var r=u(e.components);return n.createElement(c.Provider,{value:r},e.children)},m="mdxType",f={inlineCode:"code",wrapper:function(e){var r=e.children;return n.createElement(n.Fragment,{},r)}},g=n.forwardRef((function(e,r){var t=e.components,o=e.mdxType,a=e.originalType,i=e.parentName,c=p(e,["components","mdxType","originalType","parentName"]),s=u(t),d=o,m=s["".concat(i,".").concat(d)]||s[d]||f[d]||a;return t?n.createElement(m,l(l({ref:r},c),{},{components:t})):n.createElement(m,l({ref:r},c))}));function y(e,r){var t=arguments,o=r&&r.mdxType;if("string"==typeof e||o){var a=t.length,i=new Array(a);i[0]=g;var l={};for(var p in r)hasOwnProperty.call(r,p)&&(l[p]=r[p]);l.originalType=e,l[m]="string"==typeof e?e:o,i[1]=l;for(var c=2;c<a;c++)i[c]=t[c];return n.createElement.apply(null,i)}return n.createElement.apply(null,t)}g.displayName="MDXCreateElement"},65048:(e,r,t)=>{t.r(r),t.d(r,{assets:()=>p,contentTitle:()=>i,default:()=>d,frontMatter:()=>a,metadata:()=>l,toc:()=>c});var n=t(58168),o=(t(96540),t(15680));const a={sidebar_position:0},i="C1000 - No Crypto",l={unversionedId:"erlang-error-index/c/C1000",id:"erlang-error-index/c/C1000",title:"C1000 - No Crypto",description:"Error",source:"@site/docs/erlang-error-index/c/C1000.md",sourceDirName:"erlang-error-index/c",slug:"/erlang-error-index/c/C1000",permalink:"/erlang-language-platform/docs/erlang-error-index/c/C1000",draft:!1,tags:[],version:"current",sidebarPosition:0,frontMatter:{sidebar_position:0},sidebar:"tutorialSidebar",previous:{title:"About",permalink:"/erlang-language-platform/docs/erlang-error-index/c/about"},next:{title:"C1001 - Bad Crypto Key",permalink:"/erlang-language-platform/docs/erlang-error-index/c/C1001"}},p={},c=[{value:"Error",id:"error",level:2},{value:"Explanation",id:"explanation",level:2}],s={toc:c},u="wrapper";function d(e){let{components:r,...t}=e;return(0,o.mdx)(u,(0,n.A)({},s,t,{components:r,mdxType:"MDXLayout"}),(0,o.mdx)("h1",{id:"c1000---no-crypto"},"C1000 - No Crypto"),(0,o.mdx)("h2",{id:"error"},"Error"),(0,o.mdx)("pre",null,(0,o.mdx)("code",{parentName:"pre",className:"language-erlang"},"")),(0,o.mdx)("h2",{id:"explanation"},"Explanation"),(0,o.mdx)("p",null,"The current system is not configured with ",(0,o.mdx)("inlineCode",{parentName:"p"},"crypto")," support, but the code is trying to use code from the ",(0,o.mdx)("inlineCode",{parentName:"p"},"crypto")," application."),(0,o.mdx)("p",null,"The error is most likely due to Erlang being installed without SSL support. To fix the issue you should considering re-installing Erlang using the ",(0,o.mdx)("inlineCode",{parentName:"p"},"--with-ssl")," option and ensuring ",(0,o.mdx)("em",{parentName:"p"},"OpenSSL")," is available for your system. Building Erlang without OpenSSL supports causes the ",(0,o.mdx)("inlineCode",{parentName:"p"},"crypto"),", ",(0,o.mdx)("inlineCode",{parentName:"p"},"ssl")," and ",(0,o.mdx)("inlineCode",{parentName:"p"},"ssh")," applications not to work correctly and the ",(0,o.mdx)("inlineCode",{parentName:"p"},"public_key")," application to have very limited capabilities."),(0,o.mdx)("p",null,"For further information on how to install Erlang, please refer to the ",(0,o.mdx)("a",{parentName:"p",href:"https://www.erlang.org/docs/26/installation_guide/install"},"official installation instructions"),"."))}d.isMDXComponent=!0}}]);