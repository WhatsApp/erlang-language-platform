"use strict";(self.webpackChunkstaticdocs_starter=self.webpackChunkstaticdocs_starter||[]).push([[3055],{15680:(e,n,t)=>{t.r(n),t.d(n,{MDXContext:()=>c,MDXProvider:()=>d,mdx:()=>h,useMDXComponents:()=>s,withMDXComponents:()=>p});var r=t(96540);function a(e,n,t){return n in e?Object.defineProperty(e,n,{value:t,enumerable:!0,configurable:!0,writable:!0}):e[n]=t,e}function o(){return o=Object.assign||function(e){for(var n=1;n<arguments.length;n++){var t=arguments[n];for(var r in t)Object.prototype.hasOwnProperty.call(t,r)&&(e[r]=t[r])}return e},o.apply(this,arguments)}function i(e,n){var t=Object.keys(e);if(Object.getOwnPropertySymbols){var r=Object.getOwnPropertySymbols(e);n&&(r=r.filter((function(n){return Object.getOwnPropertyDescriptor(e,n).enumerable}))),t.push.apply(t,r)}return t}function l(e){for(var n=1;n<arguments.length;n++){var t=null!=arguments[n]?arguments[n]:{};n%2?i(Object(t),!0).forEach((function(n){a(e,n,t[n])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(t)):i(Object(t)).forEach((function(n){Object.defineProperty(e,n,Object.getOwnPropertyDescriptor(t,n))}))}return e}function m(e,n){if(null==e)return{};var t,r,a=function(e,n){if(null==e)return{};var t,r,a={},o=Object.keys(e);for(r=0;r<o.length;r++)t=o[r],n.indexOf(t)>=0||(a[t]=e[t]);return a}(e,n);if(Object.getOwnPropertySymbols){var o=Object.getOwnPropertySymbols(e);for(r=0;r<o.length;r++)t=o[r],n.indexOf(t)>=0||Object.prototype.propertyIsEnumerable.call(e,t)&&(a[t]=e[t])}return a}var c=r.createContext({}),p=function(e){return function(n){var t=s(n.components);return r.createElement(e,o({},n,{components:t}))}},s=function(e){var n=r.useContext(c),t=n;return e&&(t="function"==typeof e?e(n):l(l({},n),e)),t},d=function(e){var n=s(e.components);return r.createElement(c.Provider,{value:n},e.children)},u="mdxType",f={inlineCode:"code",wrapper:function(e){var n=e.children;return r.createElement(r.Fragment,{},n)}},g=r.forwardRef((function(e,n){var t=e.components,a=e.mdxType,o=e.originalType,i=e.parentName,c=m(e,["components","mdxType","originalType","parentName"]),p=s(t),d=a,u=p["".concat(i,".").concat(d)]||p[d]||f[d]||o;return t?r.createElement(u,l(l({ref:n},c),{},{components:t})):r.createElement(u,l({ref:n},c))}));function h(e,n){var t=arguments,a=n&&n.mdxType;if("string"==typeof e||a){var o=t.length,i=new Array(o);i[0]=g;var l={};for(var m in n)hasOwnProperty.call(n,m)&&(l[m]=n[m]);l.originalType=e,l[u]="string"==typeof e?e:a,i[1]=l;for(var c=2;c<o;c++)i[c]=t[c];return r.createElement.apply(null,i)}return r.createElement.apply(null,t)}g.displayName="MDXCreateElement"},92109:(e,n,t)=>{t.r(n),t.d(n,{assets:()=>m,contentTitle:()=>i,default:()=>d,frontMatter:()=>o,metadata:()=>l,toc:()=>c});var r=t(58168),a=(t(96540),t(15680));const o={sidebar_position:8},i="W0008 - Unreachable Test",l={unversionedId:"erlang-error-index/w/W0008",id:"erlang-error-index/w/W0008",title:"W0008 - Unreachable Test",description:"Error",source:"@site/docs/erlang-error-index/w/W0008.md",sourceDirName:"erlang-error-index/w",slug:"/erlang-error-index/w/W0008",permalink:"/erlang-language-platform/docs/erlang-error-index/w/W0008",draft:!1,tags:[],version:"current",sidebarPosition:8,frontMatter:{sidebar_position:8},sidebar:"tutorialSidebar",previous:{title:"W0007 - Trivial Match",permalink:"/erlang-language-platform/docs/erlang-error-index/w/W0007"},next:{title:"W0009 - Redundant Assignment",permalink:"/erlang-language-platform/docs/erlang-error-index/w/W0009"}},m={},c=[{value:"Error",id:"error",level:2},{value:"Explanation",id:"explanation",level:2}],p={toc:c},s="wrapper";function d(e){let{components:n,...t}=e;return(0,a.mdx)(s,(0,r.A)({},p,t,{components:n,mdxType:"MDXLayout"}),(0,a.mdx)("h1",{id:"w0008---unreachable-test"},"W0008 - Unreachable Test"),(0,a.mdx)("h2",{id:"error"},"Error"),(0,a.mdx)("pre",null,(0,a.mdx)("code",{parentName:"pre",className:"language-erlang"},"   -module(my_SUITE).\n   -export([all/0]).\n   -export([a/1, b/1]).\n   all() -> [a].\n   a(_Config) ->\n     ok.\n   b(_Config) ->\n%% ^ warning: Unreachable test (b/1)\n     ok.\n")),(0,a.mdx)("h2",{id:"explanation"},"Explanation"),(0,a.mdx)("p",null,"The error message is indicating that the ",(0,a.mdx)("inlineCode",{parentName:"p"},"b")," test case is not ",(0,a.mdx)("em",{parentName:"p"},"reachable")," by the ",(0,a.mdx)("a",{parentName:"p",href:"https://www.erlang.org/doc/man/common_test.html"},"Common Test")," testing framework and that it won't be executed by the testing framework."),(0,a.mdx)("p",null,"In ",(0,a.mdx)("em",{parentName:"p"},"Common Test"),", test cases are auto-discovered by the framework via the ",(0,a.mdx)("inlineCode",{parentName:"p"},"all/0")," and ",(0,a.mdx)("inlineCode",{parentName:"p"},"groups/1")," function."),(0,a.mdx)("p",null,"A function is deemed an ",(0,a.mdx)("em",{parentName:"p"},"unreachable test")," if:"),(0,a.mdx)("ul",null,(0,a.mdx)("li",{parentName:"ul"},"the function belongs to a module name ending with ",(0,a.mdx)("inlineCode",{parentName:"li"},"_SUITE")),(0,a.mdx)("li",{parentName:"ul"},"the function has arity ",(0,a.mdx)("inlineCode",{parentName:"li"},"1")),(0,a.mdx)("li",{parentName:"ul"},"the function is exported"),(0,a.mdx)("li",{parentName:"ul"},"the function is not a Common Test callback function (i.e. ",(0,a.mdx)("inlineCode",{parentName:"li"},"init_per_suite/1"),", ",(0,a.mdx)("inlineCode",{parentName:"li"},"end_per_suite/1")," or ",(0,a.mdx)("inlineCode",{parentName:"li"},"group/1"),")"),(0,a.mdx)("li",{parentName:"ul"},"the function is not a callback implementation of an included behaviour"),(0,a.mdx)("li",{parentName:"ul"},"the function is not reachable via the ",(0,a.mdx)("inlineCode",{parentName:"li"},"all/0")," and ",(0,a.mdx)("inlineCode",{parentName:"li"},"group/0")," functions and it is therefore not executed by the Erlang Common Test framework.")),(0,a.mdx)("p",null,"In the above snippet, the ",(0,a.mdx)("inlineCode",{parentName:"p"},"b/1")," function falls into that category since all of the above apply."),(0,a.mdx)("p",null,"To fix this warning you should either remove the test cases (if not necessary any longer) or make it reachable via the ",(0,a.mdx)("inlineCode",{parentName:"p"},"all/0")," function."),(0,a.mdx)("p",null,"Sometimes it is intentional to have test cases which are defined but not running (e.g. they are occasionally run manually). In such case you can silent the warning by using an ELP ",(0,a.mdx)("a",{parentName:"p",href:"/erlang-language-platform/docs/erlang-error-index/#ignoring-diagnostics"},"ignore annotation"),"."))}d.isMDXComponent=!0}}]);