define(["require","exports","tslib","react","modules/core/i18n","spectrum/modal","modules/clean/static_urls","spectrum/button","deep-integrations/text/text","modules/clean/react_format","modules/clean/ux_analytics_modal_tracking","modules/clean/react/css","modules/clean/react/modal","react-modal","external/lodash","deep-integrations/search_input/search_input","deep-integrations/search_input/clear_input_button","spectrum/colorized_icon","deep-integrations/icons/icon_cancel","modules/clean/react/extensions/common","modules/clean/react/extensions/data/helpers","modules/clean/react/app_actions/redirect","modules/clean/react/file_viewer/open_button/types","modules/clean/react/extensions/cloud_docs_compat"],function(e,t,n,a,s,o,c,r,i,l,d,u,m,p,_,f,g,h,y,x,E,v,C,S){"use strict";Object.defineProperty(t,"__esModule",{value:!0}),a=n.__importDefault(a),_=n.__importStar(_);var N=(function(e){function t(t){var n=e.call(this,t)||this;return n.hasUsedSearchBar=!1,n.createClickHandler=function(e){return function(){m.Modal.close();var t=n.props,a=t.user,s=t.file,o=t.featureFlags,c=t.currentSession,r=t.telemetryContext,i=t.updateLinkState;if(v.redirectToActionOrShowAuth(a,s,e,o,r,i),"redirect"===e.handler[".tag"]){var l=x.isLinked(e)?"select_open_with":"select_connect";x.logEvent(c,"select_action",{action_id:e.id,encoded_app_id:e.app_id,action_type:l})}else if("cloud_editor"===e.handler[".tag"]){var d=S.isWopiAction(e)?C.OpenButtonAction.OPEN_WITH:C.OpenButtonAction.OPEN_WITH_CLOUD_DOC;x.logEvent(n.props.currentSession,"select_legacy_action",{type:d})}}},n.handleQueryChange=function(e){n.setState({searchKey:e}),n.hasUsedSearchBar||(x.logEvent(n.props.currentSession,"start_searching",{}),n.hasUsedSearchBar=!0)},n.filterActions=function(){var e=n.props,t=e.appActions,a=e.categoryIdToInfos,s=n.state.searchKey.trim().toLowerCase(),o=t.map(function(e){return e.category}),c=o.map(function(e){return a[e]}),r=c.filter(function(e){return e.display_name.toLowerCase().indexOf(s)!==-1}),i=r.map(function(e){return e.id});return t.filter(function(e){return e.description.toLowerCase().indexOf(s)!==-1||i.includes(e.category)})},n.renderExtensionItem=function(e){var t=n.props.categoryIdToInfos,o=e.icon,i=o.is_static?c.static_url("/static/images/generic_app_icon-vflIPYT1H.png"):o.url,l=e.app_copy,d=x.isLinked(e),u=d?"primary":"secondary",m=d?s._("Open with"):s._("Connect"),p=t[e.category];return a.default.createElement("div",{className:"extension-section",key:e.description},a.default.createElement("div",{className:"extension-tile"},a.default.createElement("img",{src:i,className:"extensions-icon",alt:""}),a.default.createElement("div",{className:"extensions-description"},a.default.createElement("div",{className:"extensions-category-name"},e.description),a.default.createElement("div",{className:"extensions-app-name"},p.display_name)),a.default.createElement(r.Button,{variant:u,className:"extensions-scaling-modal__redirect_button",onClick:n.createClickHandler(e)},m)),a.default.createElement("div",{className:"extensions-scaling-modal__app-description"},l&&l.map(function(e,t){return a.default.createElement("p",{key:t,className:"extensions-scaling-modal__app-description-line"},"• "+e)})))},n.renderEmptySearch=function(){return a.default.createElement("div",{className:"extensions-scaling-modal__empty-search"},a.default.createElement("img",{className:"extensions-scaling-modal__empty-search-image",src:c.static_url("/static/images/empty_states/search-vflHMNVT9.png"),srcSet:c.static_url("/static/images/empty_states/search@2x-vfly2B3kk.png")+" 2x",alt:""}),a.default.createElement(i.Text,{size:"medium"},l.reactFormat(s._("<bold>Looking for something?</bold>"),{bold:a.default.createElement(i.Text,{fontWeight:"medium",size:"large"})})),a.default.createElement(i.Text,{size:"medium"},s._("Check spelling or try again.")))},n.state={searchKey:""},n}return n.__extends(t,e),t.prototype.render=function(){var e=this,t=this.props,n=t.categoryIdToInfos,c=t.onRequestClose,r=s._("Apps"),l=E.constructAlphabetizeActions(n),u=this.filterActions().sort(l);return a.default.createElement(o.Modal,{ariaLabel:s._("App actions"),className:"extensions-scaling-modal",displayCloseButton:!0,open:!0,overlayClassName:"file-viewer-modal-overlay",overlayFixed:!1,onRequestClose:function(){return c()}},a.default.createElement("div",{className:"extensions-scaling-modal__body-wrapper"},a.default.createElement("div",{className:"extensions-scaling-modal__title"},a.default.createElement(i.Text,{size:"large"},r)),a.default.createElement("div",{className:"extensions-scaling-modal__search-bar"},a.default.createElement(f.SearchInput,{clearButtonRenderer:function(e){var t=e.handleClick,n=e.handleKeyDown,s=e.handleMouseDown;return a.default.createElement(g.ClearInputButton,{icon:a.default.createElement(h.ColorizedIcon,{color:"#707781"},a.default.createElement(y.IconCancel,null)),onClick:t,onKeyDown:n,onMouseDown:s})},keyboardShortcut:"",placeholder:s._("Search by action or app name"),value:this.state.searchKey,onChange:this.handleQueryChange})),a.default.createElement("div",{className:"extensions-container"},u.length>0?u.map(function(t){return e.renderExtensionItem(t)}):this.renderEmptySearch())),a.default.createElement(d.UXAnalyticsModalTracking,{id:"extensions-scaling-modal"}))},t})(a.default.Component);t.ExtensionsScalingModal=u.requireCssWithComponent(N,["/static/js/deep-integrations/index.web-vflFndkIa.css","/static/css/app_actions/index-vfl_lauE_.css"]);var b=_.once(function(){p.setAppElement(document.body)});t.showExtensionsScalingModal=function(e,n,s,o,c,r,i,l){b(),m.Modal.showInstance(a.default.createElement(t.ExtensionsScalingModal,{user:e,file:n,onRequestClose:m.Modal.close,appActions:s,categoryIdToInfos:o,featureFlags:c,updateLinkState:r,telemetryContext:i,currentSession:l}));var d=x.partitionActionsByLinkStatus(s),u=d.connected_apps,p=d.unconnected_apps;x.logEvent(l,"view_scaling_modal",{connected_apps:u,unconnected_apps:p})}});
//# sourceMappingURL=extensions_scaling_modal.min.js-vflOCvPJp.map