define(["require","exports","tslib","react","comments2/components/comment_editor/components/cancel_button","comments2/components/comment_editor/components/post_button"],function(e,t,n,o,c,l){"use strict";Object.defineProperty(t,"__esModule",{value:!0}),o=n.__importStar(o),t.PostBar=o.memo(function(e){var t=e.children,n=e.cancelEnabled,s=e.cancelLabel,a=e.postEnabled,m=e.postLabel,r=e.signals;return o.createElement("div",{className:"sc-comment-editor-controls-container"},o.createElement("div",{className:"sc-comment-editor-controls"},o.createElement(l.PostButton,{label:m,disabled:!a,onClick:r.post}),o.createElement(c.CancelButton,{label:s,disabled:!n,onClick:r.cancel}),o.createElement("div",{className:"sc-comment-editor-controls-right"},t)))})});
//# sourceMappingURL=post_bar.min.js-vflxFpwKv.map