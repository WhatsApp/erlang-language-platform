"use strict";(self.webpackChunkstaticdocs_starter=self.webpackChunkstaticdocs_starter||[]).push([[5677],{51836:(e,r,n)=>{n.r(r),n.d(r,{assets:()=>l,contentTitle:()=>s,default:()=>u,frontMatter:()=>i,metadata:()=>t,toc:()=>c});const t=JSON.parse('{"id":"erlang-error-index/l/L1317","title":"L1317 - Format Error","description":"Error","source":"@site/docs/erlang-error-index/l/L1317.md","sourceDirName":"erlang-error-index/l","slug":"/erlang-error-index/l/L1317","permalink":"/erlang-language-platform/docs/erlang-error-index/l/L1317","draft":false,"unlisted":false,"tags":[],"version":"current","sidebarPosition":317,"frontMatter":{"sidebar_position":317},"sidebar":"tutorialSidebar","previous":{"title":"L1309 - Missing function specification","permalink":"/erlang-language-platform/docs/erlang-error-index/l/L1309"},"next":{"title":"L1318 - Expression Updates a Literal","permalink":"/erlang-language-platform/docs/erlang-error-index/l/L1318"}}');var o=n(74848),a=n(28453);const i={sidebar_position:317},s="L1317 - Format Error",l={},c=[{value:"Error",id:"error",level:2},{value:"Explanation",id:"explanation",level:2}];function d(e){const r={a:"a",code:"code",h1:"h1",h2:"h2",header:"header",p:"p",pre:"pre",strong:"strong",...(0,a.R)(),...e.components};return(0,o.jsxs)(o.Fragment,{children:[(0,o.jsx)(r.header,{children:(0,o.jsx)(r.h1,{id:"l1317---format-error",children:"L1317 - Format Error"})}),"\n",(0,o.jsx)(r.h2,{id:"error",children:"Error"}),"\n",(0,o.jsx)(r.pre,{children:(0,o.jsx)(r.code,{className:"language-erlang",children:'  main() ->\n  io:format("These are two arguments: ~p, ~p", [only_one]).\n%%                                             ^^^^^^^^^^ warning: the format string requires an argument list with 2 arguments, but the argument list contains only 1 argument\n'})}),"\n",(0,o.jsx)(r.h2,{id:"explanation",children:"Explanation"}),"\n",(0,o.jsxs)(r.p,{children:["The warning occurs when the format string and the actual list of parameters in a ",(0,o.jsx)(r.code,{children:"io:format/2"}),", ",(0,o.jsx)(r.code,{children:"io_lib:format/2"})," or equivalent function are inconsistent. In the example above, this happens because the format string (the first of the two arguments to the ",(0,o.jsx)(r.code,{children:"io:format/2"})," function) contains ",(0,o.jsx)(r.strong,{children:"two"})," control sequences for formatting (",(0,o.jsx)(r.code,{children:"~p"}),"), but the list of arguments only contains ",(0,o.jsx)(r.strong,{children:"one"})," element."]}),"\n",(0,o.jsxs)(r.p,{children:["To learn more about formatting strings and control sequences, please refer to the official documentation for the ",(0,o.jsx)(r.a,{href:"https://www.erlang.org/doc/apps/stdlib/io.html#fwrite/3",children:(0,o.jsx)(r.code,{children:"io:fwrite/3"})})," function."]})]})}function u(e={}){const{wrapper:r}={...(0,a.R)(),...e.components};return r?(0,o.jsx)(r,{...e,children:(0,o.jsx)(d,{...e})}):d(e)}},28453:(e,r,n)=>{n.d(r,{R:()=>i,x:()=>s});var t=n(96540);const o={},a=t.createContext(o);function i(e){const r=t.useContext(a);return t.useMemo((function(){return"function"==typeof e?e(r):{...r,...e}}),[r,e])}function s(e){let r;return r=e.disableParentContext?"function"==typeof e.components?e.components(o):e.components||o:i(e.components),t.createElement(a.Provider,{value:r},e.children)}}}]);