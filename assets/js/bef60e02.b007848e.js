"use strict";(self.webpackChunkstaticdocs_starter=self.webpackChunkstaticdocs_starter||[]).push([[8551],{3905:(e,t,n)=>{n.r(t),n.d(t,{MDXContext:()=>s,MDXProvider:()=>c,mdx:()=>g,useMDXComponents:()=>p,withMDXComponents:()=>d});var r=n(67294);function a(e,t,n){return t in e?Object.defineProperty(e,t,{value:n,enumerable:!0,configurable:!0,writable:!0}):e[t]=n,e}function o(){return o=Object.assign||function(e){for(var t=1;t<arguments.length;t++){var n=arguments[t];for(var r in n)Object.prototype.hasOwnProperty.call(n,r)&&(e[r]=n[r])}return e},o.apply(this,arguments)}function i(e,t){var n=Object.keys(e);if(Object.getOwnPropertySymbols){var r=Object.getOwnPropertySymbols(e);t&&(r=r.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),n.push.apply(n,r)}return n}function l(e){for(var t=1;t<arguments.length;t++){var n=null!=arguments[t]?arguments[t]:{};t%2?i(Object(n),!0).forEach((function(t){a(e,t,n[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(n)):i(Object(n)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(n,t))}))}return e}function m(e,t){if(null==e)return{};var n,r,a=function(e,t){if(null==e)return{};var n,r,a={},o=Object.keys(e);for(r=0;r<o.length;r++)n=o[r],t.indexOf(n)>=0||(a[n]=e[n]);return a}(e,t);if(Object.getOwnPropertySymbols){var o=Object.getOwnPropertySymbols(e);for(r=0;r<o.length;r++)n=o[r],t.indexOf(n)>=0||Object.prototype.propertyIsEnumerable.call(e,n)&&(a[n]=e[n])}return a}var s=r.createContext({}),d=function(e){return function(t){var n=p(t.components);return r.createElement(e,o({},t,{components:n}))}},p=function(e){var t=r.useContext(s),n=t;return e&&(n="function"==typeof e?e(t):l(l({},t),e)),n},c=function(e){var t=p(e.components);return r.createElement(s.Provider,{value:t},e.children)},u="mdxType",x={inlineCode:"code",wrapper:function(e){var t=e.children;return r.createElement(r.Fragment,{},t)}},f=r.forwardRef((function(e,t){var n=e.components,a=e.mdxType,o=e.originalType,i=e.parentName,s=m(e,["components","mdxType","originalType","parentName"]),d=p(n),c=a,u=d["".concat(i,".").concat(c)]||d[c]||x[c]||o;return n?r.createElement(u,l(l({ref:t},s),{},{components:n})):r.createElement(u,l({ref:t},s))}));function g(e,t){var n=arguments,a=t&&t.mdxType;if("string"==typeof e||a){var o=n.length,i=new Array(o);i[0]=f;var l={};for(var m in t)hasOwnProperty.call(t,m)&&(l[m]=t[m]);l.originalType=e,l[u]="string"==typeof e?e:a,i[1]=l;for(var s=2;s<o;s++)i[s]=n[s];return r.createElement.apply(null,i)}return r.createElement.apply(null,n)}f.displayName="MDXCreateElement"},9071:(e,t,n)=>{n.r(t),n.d(t,{assets:()=>m,contentTitle:()=>i,default:()=>c,frontMatter:()=>o,metadata:()=>l,toc:()=>s});var r=n(87462),a=(n(67294),n(3905));const o={sidebar_position:23},i="W0023 - Risk of Atoms Exhaustion",l={unversionedId:"erlang-error-index/w/W0023",id:"erlang-error-index/w/W0023",title:"W0023 - Risk of Atoms Exhaustion",description:"Error",source:"@site/docs/erlang-error-index/w/W0023.md",sourceDirName:"erlang-error-index/w",slug:"/erlang-error-index/w/W0023",permalink:"/erlang-language-platform/docs/erlang-error-index/w/W0023",draft:!1,tags:[],version:"current",sidebarPosition:23,frontMatter:{sidebar_position:23},sidebar:"tutorialSidebar",previous:{title:"W0022 - Missing no_link option in meck:new invocation",permalink:"/erlang-language-platform/docs/erlang-error-index/w/W0022"}},m={},s=[{value:"Error",id:"error",level:2},{value:"Explanation",id:"explanation",level:2}],d={toc:s},p="wrapper";function c(e){let{components:t,...n}=e;return(0,a.mdx)(p,(0,r.Z)({},d,n,{components:t,mdxType:"MDXLayout"}),(0,a.mdx)("h1",{id:"w0023---risk-of-atoms-exhaustion"},"W0023 - Risk of Atoms Exhaustion"),(0,a.mdx)("h2",{id:"error"},"Error"),(0,a.mdx)("pre",null,(0,a.mdx)("code",{parentName:"pre",className:"language-erlang"},"   -module(main).\n\n   -export([do/0]).\n\n   do() ->\n     [binary_to_atom(<<I/integer>>) || I <- lists:seq(1, 100)].\n   %% ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ \ud83d\udca1 error: Risk of atoms exhaustion.\n")),(0,a.mdx)("h2",{id:"explanation"},"Explanation"),(0,a.mdx)("p",null,"Erlang atoms are not garbage-collected. Once an atom is created, it is never removed. The emulator terminates if the ",(0,a.mdx)("a",{parentName:"p",href:"https://www.erlang.org/doc/efficiency_guide/advanced#system-limits"},"configurable limit")," for the number of atoms is reached."),(0,a.mdx)("p",null,"Therefore, converting arbitrary input strings or binaries to atoms can be dangerous in a system that runs continuously. All functions which can create atoms have a ",(0,a.mdx)("em",{parentName:"p"},"safe")," variant which should be favoured when possible:"),(0,a.mdx)("table",null,(0,a.mdx)("thead",{parentName:"table"},(0,a.mdx)("tr",{parentName:"thead"},(0,a.mdx)("th",{parentName:"tr",align:null},"Conversion Function"),(0,a.mdx)("th",{parentName:"tr",align:null},"Safer Version"))),(0,a.mdx)("tbody",{parentName:"table"},(0,a.mdx)("tr",{parentName:"tbody"},(0,a.mdx)("td",{parentName:"tr",align:null},(0,a.mdx)("inlineCode",{parentName:"td"},"list_to_atom/1")),(0,a.mdx)("td",{parentName:"tr",align:null},(0,a.mdx)("inlineCode",{parentName:"td"},"list_to_existing_atom/1"))),(0,a.mdx)("tr",{parentName:"tbody"},(0,a.mdx)("td",{parentName:"tr",align:null},(0,a.mdx)("inlineCode",{parentName:"td"},"binary_to_atom/1,2")),(0,a.mdx)("td",{parentName:"tr",align:null},(0,a.mdx)("inlineCode",{parentName:"td"},"binary_to_existing_atom/1,2"))),(0,a.mdx)("tr",{parentName:"tbody"},(0,a.mdx)("td",{parentName:"tr",align:null},(0,a.mdx)("inlineCode",{parentName:"td"},"binaty_to_term/1,2")),(0,a.mdx)("td",{parentName:"tr",align:null},(0,a.mdx)("inlineCode",{parentName:"td"},"binary_to_term/2")," with the ",(0,a.mdx)("inlineCode",{parentName:"td"},"safe")," option")))),(0,a.mdx)("p",null,"When using the ",(0,a.mdx)("em",{parentName:"p"},"safe")," or ",(0,a.mdx)("em",{parentName:"p"},"existing")," versions of the above conversion functions, all atoms ",(0,a.mdx)("strong",{parentName:"p"},"must have been created earlier")," or the function will cause an exception:"),(0,a.mdx)("pre",null,(0,a.mdx)("code",{parentName:"pre"},'1> binary_to_existing_atom(<<"this_atom_does_not_exist">>).\n** exception error: bad argument\n     in function  binary_to_existing_atom/1\n        called as binary_to_existing_atom(<<"this_atom_does_not_exist">>)\n        *** argument 1: not an already existing atom\n')),(0,a.mdx)("p",null,"To explicitly create necessary atoms beforehand, you can export a function that returns a list of all atoms which are expected:"),(0,a.mdx)("pre",null,(0,a.mdx)("code",{parentName:"pre"},"-export([atoms/0]).\natoms() ->\n  [my_atom, your_atom, our_atom].\n")),(0,a.mdx)("p",null,"If there is no way to know atom names in advance and there is ",(0,a.mdx)("strong",{parentName:"p"},"100% confidence in bounded string variations passed to the conversion function")," you can preceed the conversion function call with a special comment to silent the linter:"),(0,a.mdx)("pre",null,(0,a.mdx)("code",{parentName:"pre"},"% elp:ignore atoms_exhaustion - An optional explanation here\n")),(0,a.mdx)("p",null,"For more information see the ",(0,a.mdx)("a",{parentName:"p",href:"https://erlef.github.io/security-wg/secure_coding_and_deployment_hardening/atom_exhaustion"},"Atom Exhausion")," section of the Erlang Ecosystem Foundation Security Work Group."))}c.isMDXComponent=!0}}]);