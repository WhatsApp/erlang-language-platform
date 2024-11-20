"use strict";(self.webpackChunkstaticdocs_starter=self.webpackChunkstaticdocs_starter||[]).push([[6559],{50032:(e,n,r)=>{r.r(n),r.d(n,{assets:()=>o,contentTitle:()=>l,default:()=>h,frontMatter:()=>s,metadata:()=>t,toc:()=>d});const t=JSON.parse('{"id":"erlang-error-index/w/W0010","title":"W0010 - Unused Function Argument","description":"Error","source":"@site/docs/erlang-error-index/w/W0010.md","sourceDirName":"erlang-error-index/w","slug":"/erlang-error-index/w/W0010","permalink":"/erlang-language-platform/docs/erlang-error-index/w/W0010","draft":false,"unlisted":false,"tags":[],"version":"current","sidebarPosition":10,"frontMatter":{"sidebar_position":10},"sidebar":"tutorialSidebar","previous":{"title":"W0009 - Redundant Assignment","permalink":"/erlang-language-platform/docs/erlang-error-index/w/W0009"},"next":{"title":"W0011 - Application Get Env","permalink":"/erlang-language-platform/docs/erlang-error-index/w/W0011"}}');var a=r(74848),i=r(28453);const s={sidebar_position:10},l="W0010 - Unused Function Argument",o={},d=[{value:"Error",id:"error",level:2},{value:"Explanation",id:"explanation",level:2}];function c(e){const n={code:"code",h1:"h1",h2:"h2",header:"header",li:"li",p:"p",pre:"pre",ul:"ul",...(0,i.R)(),...e.components};return(0,a.jsxs)(a.Fragment,{children:[(0,a.jsx)(n.header,{children:(0,a.jsx)(n.h1,{id:"w0010---unused-function-argument",children:"W0010 - Unused Function Argument"})}),"\n",(0,a.jsx)(n.h2,{id:"error",children:"Error"}),"\n",(0,a.jsx)(n.pre,{children:(0,a.jsx)(n.code,{className:"language-erlang",children:"length([]) -> 0;\nlength([Head|Tail]) -> 1 + length(Tail).\n     %% ^^^^ warning: this variable is unused\n"})}),"\n",(0,a.jsx)(n.h2,{id:"explanation",children:"Explanation"}),"\n",(0,a.jsx)(n.p,{children:"The message is indicating that the given variable is unused within the function clause body. To fix the warning you should either:"}),"\n",(0,a.jsxs)(n.ul,{children:["\n",(0,a.jsx)(n.li,{children:"remove the unused parameter"}),"\n",(0,a.jsxs)(n.li,{children:["replace it with an underscore (",(0,a.jsx)(n.code,{children:"_"}),")"]}),"\n",(0,a.jsxs)(n.li,{children:["prepend it with an underscore (",(0,a.jsx)(n.code,{children:"_"}),")"]}),"\n"]}),"\n",(0,a.jsxs)(n.p,{children:["In the above snippet, we don't need to look at the actual value of the ",(0,a.jsx)(n.code,{children:"Head"})," to calculate the length of the input list. Therefore, we can replace the variable name with an underscore:"]}),"\n",(0,a.jsx)(n.pre,{children:(0,a.jsx)(n.code,{className:"language-erlang",children:"length([]) -> 0;\nlength([_|Tail]) -> 1 + length(Tail).\n"})}),"\n",(0,a.jsx)(n.p,{children:"Even better, we could keep the variable name and prepend it with an underscore. This way we'll get the best of both worlds: we will silent the warning, while keeping the code readable:"}),"\n",(0,a.jsx)(n.pre,{children:(0,a.jsx)(n.code,{className:"language-erlang",children:"length([]) -> 0;\nlength([_Head|Tail]) -> 1 + length(Tail).\n"})}),"\n",(0,a.jsx)(n.p,{children:"Let's look at a different scenario:"}),"\n",(0,a.jsx)(n.pre,{children:(0,a.jsx)(n.code,{className:"language-erlang",children:"handle_message(hi, State) ->\n                %% ^^^^^ warning: this variable is unused\n    hi();\nhandle_message(bye, State) ->\n                 %% ^^^^^ warning: this variable is unused\n    bye().\n"})}),"\n",(0,a.jsxs)(n.p,{children:["In this case the ",(0,a.jsx)(n.code,{children:"State"})," variable is not needed by any of the function clauses of the ",(0,a.jsx)(n.code,{children:"handle_message/2"})," function. Therefore, we could consider simplifying the function signature getting rid of the un-necessary second argument:"]}),"\n",(0,a.jsx)(n.pre,{children:(0,a.jsx)(n.code,{className:"language-erlang",children:"handle_message(hi) ->\n    hi();\nhandle_message(bye) ->\n    bye().\n"})}),"\n",(0,a.jsxs)(n.p,{children:["Keep in mind that in this case we may need to update callers of the ",(0,a.jsx)(n.code,{children:"handle_message/1"})," function!"]})]})}function h(e={}){const{wrapper:n}={...(0,i.R)(),...e.components};return n?(0,a.jsx)(n,{...e,children:(0,a.jsx)(c,{...e})}):c(e)}},28453:(e,n,r)=>{r.d(n,{R:()=>s,x:()=>l});var t=r(96540);const a={},i=t.createContext(a);function s(e){const n=t.useContext(i);return t.useMemo((function(){return"function"==typeof e?e(n):{...n,...e}}),[n,e])}function l(e){let n;return n=e.disableParentContext?"function"==typeof e.components?e.components(a):e.components||a:s(e.components),t.createElement(i.Provider,{value:n},e.children)}}}]);