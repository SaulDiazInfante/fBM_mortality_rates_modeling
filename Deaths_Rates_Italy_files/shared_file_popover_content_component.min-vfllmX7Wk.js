define(["require","exports","tslib","react","modules/clean/react/extensions/common","modules/clean/react/extensions/cloud_docs_compat","modules/clean/react/css","modules/clean/react/file_viewer/open_button/types","modules/clean/cloud_docs/open_with_utils","modules/clean/react/extensions/unity_and_cloud_editors","modules/clean/react/file_viewer/constants","modules/clean/cloud_docs/types"],function(e,t,o,n,i,r,s,c,l,p,a,u){"use strict";Object.defineProperty(t,"__esModule",{value:!0}),n=o.__importDefault(n);var d=(function(e){function t(t){var o=e.call(this,t)||this;return o.createActionClickHandler=function(e){return function(){if("cloud_editor"===e.handler[".tag"]){var t=r.isWopiAction(e)?c.OpenButtonAction.OPEN_WITH:c.OpenButtonAction.OPEN_WITH_CLOUD_DOC;i.logEvent(o.props.currentSession,"select_legacy_action",{type:t});var n=u.UserActionSourceType.WEB_PREVIEW,s=r.cloudEditorNameToParams(e.handler.editor_name);l.openWithCloudEditor(o.props.file,o.props.user,s,!1,n)}}},o}return o.__extends(t,e),t.prototype.render=function(){var e=this;if(0===this.props.openOptions.length&&0===this.props.cloudEditorAppActions.length)return null;var t=this.props.cloudEditorAppActions.map(function(t){return{appAction:t,handler:e.createActionClickHandler(t),userAction:a.UserAction.OpenWithAppAction,actionName:t.description}});return n.default.createElement(p.UnityAndCloudEditors,{unityOptions:[],legacyCloudEditorOptions:this.props.openOptions,cloudEditorAppActions:t,bylines:{}})},t})(n.default.Component);t.SharedFilePopoverContentComponent=d,t.SharedFilePopoverContent=s.requireCssWithComponent(d,["/static/css/app_actions/index-vfl_lauE_.css"])});
//# sourceMappingURL=shared_file_popover_content_component.min.js-vflvAddbv.map