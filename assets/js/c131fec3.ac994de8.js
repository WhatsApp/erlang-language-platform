"use strict";(self.webpackChunkstaticdocs_starter=self.webpackChunkstaticdocs_starter||[]).push([[7806],{15680:(e,n,t)=>{t.r(n),t.d(n,{MDXContext:()=>s,MDXProvider:()=>d,mdx:()=>x,useMDXComponents:()=>m,withMDXComponents:()=>c});var r=t(96540);function o(e,n,t){return n in e?Object.defineProperty(e,n,{value:t,enumerable:!0,configurable:!0,writable:!0}):e[n]=t,e}function i(){return i=Object.assign||function(e){for(var n=1;n<arguments.length;n++){var t=arguments[n];for(var r in t)Object.prototype.hasOwnProperty.call(t,r)&&(e[r]=t[r])}return e},i.apply(this,arguments)}function a(e,n){var t=Object.keys(e);if(Object.getOwnPropertySymbols){var r=Object.getOwnPropertySymbols(e);n&&(r=r.filter((function(n){return Object.getOwnPropertyDescriptor(e,n).enumerable}))),t.push.apply(t,r)}return t}function l(e){for(var n=1;n<arguments.length;n++){var t=null!=arguments[n]?arguments[n]:{};n%2?a(Object(t),!0).forEach((function(n){o(e,n,t[n])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(t)):a(Object(t)).forEach((function(n){Object.defineProperty(e,n,Object.getOwnPropertyDescriptor(t,n))}))}return e}function p(e,n){if(null==e)return{};var t,r,o=function(e,n){if(null==e)return{};var t,r,o={},i=Object.keys(e);for(r=0;r<i.length;r++)t=i[r],n.indexOf(t)>=0||(o[t]=e[t]);return o}(e,n);if(Object.getOwnPropertySymbols){var i=Object.getOwnPropertySymbols(e);for(r=0;r<i.length;r++)t=i[r],n.indexOf(t)>=0||Object.prototype.propertyIsEnumerable.call(e,t)&&(o[t]=e[t])}return o}var s=r.createContext({}),c=function(e){return function(n){var t=m(n.components);return r.createElement(e,i({},n,{components:t}))}},m=function(e){var n=r.useContext(s),t=n;return e&&(t="function"==typeof e?e(n):l(l({},n),e)),t},d=function(e){var n=m(e.components);return r.createElement(s.Provider,{value:n},e.children)},u="mdxType",f={inlineCode:"code",wrapper:function(e){var n=e.children;return r.createElement(r.Fragment,{},n)}},g=r.forwardRef((function(e,n){var t=e.components,o=e.mdxType,i=e.originalType,a=e.parentName,s=p(e,["components","mdxType","originalType","parentName"]),c=m(t),d=o,u=c["".concat(a,".").concat(d)]||c[d]||f[d]||i;return t?r.createElement(u,l(l({ref:n},s),{},{components:t})):r.createElement(u,l({ref:n},s))}));function x(e,n){var t=arguments,o=n&&n.mdxType;if("string"==typeof e||o){var i=t.length,a=new Array(i);a[0]=g;var l={};for(var p in n)hasOwnProperty.call(n,p)&&(l[p]=n[p]);l.originalType=e,l[u]="string"==typeof e?e:o,a[1]=l;for(var s=2;s<i;s++)a[s]=t[s];return r.createElement.apply(null,a)}return r.createElement.apply(null,t)}g.displayName="MDXCreateElement"},13385:(e,n,t)=>{t.r(n),t.d(n,{assets:()=>p,contentTitle:()=>a,default:()=>d,frontMatter:()=>i,metadata:()=>l,toc:()=>s});var r=t(58168),o=(t(96540),t(15680));const i={sidebar_position:22},a="W0022 - Missing no_link option in meck:new invocation",l={unversionedId:"erlang-error-index/w/W0022",id:"erlang-error-index/w/W0022",title:"W0022 - Missing no_link option in meck:new invocation",description:"Error",source:"@site/docs/erlang-error-index/w/W0022.md",sourceDirName:"erlang-error-index/w",slug:"/erlang-error-index/w/W0022",permalink:"/erlang-language-platform/docs/erlang-error-index/w/W0022",draft:!1,tags:[],version:"current",sidebarPosition:22,frontMatter:{sidebar_position:22},sidebar:"tutorialSidebar",previous:{title:"W0021 - Cannot Evaluate Common Test Callbacks",permalink:"/erlang-language-platform/docs/erlang-error-index/w/W0021"},next:{title:"W0023 - Risk of Atoms Exhaustion",permalink:"/erlang-language-platform/docs/erlang-error-index/w/W0023"}},p={},s=[{value:"Error",id:"error",level:2},{value:"Explanation",id:"explanation",level:2}],c={toc:s},m="wrapper";function d(e){let{components:n,...t}=e;return(0,o.mdx)(m,(0,r.A)({},c,t,{components:n,mdxType:"MDXLayout"}),(0,o.mdx)("h1",{id:"w0022---missing-no_link-option-in-mecknew-invocation"},"W0022 - Missing ",(0,o.mdx)("inlineCode",{parentName:"h1"},"no_link")," option in ",(0,o.mdx)("inlineCode",{parentName:"h1"},"meck:new")," invocation"),(0,o.mdx)("h2",{id:"error"},"Error"),(0,o.mdx)("pre",null,(0,o.mdx)("code",{parentName:"pre",className:"language-erlang"},"   -module(my_SUITE).\n\n   -export([all/0, init_per_suite/1]).\n   -export([a/1]).\n\n   all() -> [a].\n\n   init_per_suite(Config) ->\n     meck:new(my_module, [passthrough]),\n  %% ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ \ud83d\udca1 warning: Missing no_link option.\n     Config.\n\n   a(_Config) ->\n     ok.\n")),(0,o.mdx)("h2",{id:"explanation"},"Explanation"),(0,o.mdx)("p",null,"In ",(0,o.mdx)("a",{parentName:"p",href:"https://www.erlang.org/doc/apps/common_test/introduction"},"Common Test"),", every test case is executed by a dedicated Erlang process. The ",(0,o.mdx)("inlineCode",{parentName:"p"},"init_per_suite/1")," and ",(0,o.mdx)("inlineCode",{parentName:"p"},"init_per_group/2")," configuration functions are executed in separate processes. Every ",(0,o.mdx)("em",{parentName:"p"},"linked")," process spawned in those functions will be killed once the function stops executing."),(0,o.mdx)("p",null,"Unless the ",(0,o.mdx)("inlineCode",{parentName:"p"},"no_link")," option is passed to the ",(0,o.mdx)("inlineCode",{parentName:"p"},"meck:new/1,2")," invocations, the spawned process is linked, so the mock would stop working before (or while) a testcase is executing, often leading to flakyness."),(0,o.mdx)("p",null,"To fix this issue, pass the ",(0,o.mdx)("inlineCode",{parentName:"p"},"no_link")," option to the ",(0,o.mdx)("inlineCode",{parentName:"p"},"meck:new/1,2")," invocation or activate the mock outside of the ",(0,o.mdx)("inlineCode",{parentName:"p"},"init_per_suite/1")," and ",(0,o.mdx)("inlineCode",{parentName:"p"},"init_per_group/2")," functions."),(0,o.mdx)("p",null,"For more information, please refer to the ",(0,o.mdx)("a",{parentName:"p",href:"https://www.erlang.org/doc/apps/common_test/write_test_chapter#execution-environment"},"official documentation"),"."))}d.isMDXComponent=!0}}]);