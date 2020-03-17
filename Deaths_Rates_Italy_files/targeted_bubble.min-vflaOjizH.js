define(["require","exports","tslib","jquery","react","external/classnames","modules/clean/react/prompt/prompt_location","modules/clean/react/prompt/button","modules/clean/animation","modules/clean/upsell/upsell_controller","modules/clean/react/css","modules/clean/react/sprite","modules/core/i18n"],function(e,t,n,o,a,r,i,s,l,u,p,c,d){"use strict";Object.defineProperty(t,"__esModule",{value:!0}),o=n.__importDefault(o),a=n.__importDefault(a),r=n.__importDefault(r),u=n.__importDefault(u);var b={bottom:20,left:-20,right:39,top:-39},m=(function(e){function t(t,i){var p=e.call(this,t,i)||this;return p.getTargetedElement=function(){return o.default(p.props.campaign.content.targetSelector).first()},p.isTargetedElementVisible=function(){var e=p.getTargetedElement();return e.length>0&&e.is(":visible")},p.isTargetInDocument=function(e){return document.body.contains(e[0])},p.getPromptSilo=function(){return o.default("#prompt-silo")},p.waitForElementThenSetupAndPosition=function(e){if(void 0===e&&(e=Date.now()),p.isTargetedElementVisible())return void p.setupControlsAndPositionBubble();setTimeout(function(){return p.waitForElementThenSetupAndPosition(e)},200)},p.baseOnClick=function(e){null!=p.repositioner&&p.repositioner.stop(),p.setState({isShown:!1}),void 0!==p.$backdropElements&&p.$backdropElements.backdrop.remove(),p.$allJQueryButtons&&p.$allJQueryButtons.off("click",p.jqueryOnClickHandler)},p.jqueryOnClickHandler=function(e){p.baseOnClick(e),"#"===o.default(e.currentTarget).attr("href")&&e.preventDefault()},p.onConfirm=function(e){p.state_manager.onConfirm(e),p.baseOnClick(e)},p.onDismiss=function(e){p.state_manager.onDismiss(e),p.baseOnClick(e)},p.buildUpsellControllerOptions=function(){return n.__assign({},p.props,p.props.campaign,p.props.campaign.content)},p.setupControlsAndPositionBubble=function(){var e,t=p.getTargetedElement(),n=t;p.props.campaign.content.hasBackdrop&&(p.$backdropElements=p.setupAndPositionBackdrop(t),n=n.add(p.$backdropElements.backdropHole),e=p.$backdropElements.backdrop),e=e||o.default(),new u.default(n,e,p.buildUpsellControllerOptions()),p.$allJQueryButtons=n.add(e),p.$allJQueryButtons.on("click",p.jqueryOnClickHandler),p.state_manager.onShow(),p.setState({},function(){var e=t;p.props.campaign.content.hasBackdrop&&p.$backdropElements&&(e=p.$backdropElements.backdropHole);var n=function(){return p.setState({targetedBubbleWrapperZIndex:p._elementZIndex(e)+1}),p.positionBubbleAbsolute(e)};if(n(),!p.props.campaign.content.hasBackdrop)return p.repositioner=new l.RenderAtFps(n,60),p.repositioner.start()})},p.isTargetFixed=function(e){return e.parents().filter(function(){return"fixed"===o.default(this).css("position")}).length>0},p.getTargetedBubbleImage=function(){var e=p.props.campaign.content,t=e.confirmText,n=e.imageUrl;if(n)return a.default.createElement("div",{className:"targeted-bubble__image"},a.default.createElement("img",{className:"image",src:n,alt:t||void 0}))},p.getBubbleWrapperClassName=function(){var e={"targeted-bubble-wrapper":!0};return p.props.isPreview?(e.active=!0,e.static=!0,r.default(e)):(e.active=p.state.isShown,r.default(e))},p._elementZIndex=function(e){for(;e.length&&e[0]!==document;){var t=e.css("position");if(["absolute","relative","fixed"].includes(t)){var n=parseInt(e.css("zIndex"),10);if(!isNaN(n)&&0!==n)return n}e=e.parent()}return 0},p._elementDocumentRelativeRect=function(e){var t=e.getBoundingClientRect(),n=window.pageXOffset,o=window.pageYOffset;return{top:t.top+o,left:t.left+n,width:t.width,height:t.height}},p.positionBubbleAbsolute=function(e){if(p.props.campaign.content.hasBackdrop||(e=p.getTargetedElement()),!p.isTargetInDocument(e)||!p.targetedBubbleWrapperRef.current)return p.setState({isShown:!1}),void(null!=p.repositioner&&p.repositioner.stop());var t=p.props.campaign.content.placement,n=p._elementDocumentRelativeRect(e[0]),a=p.targetedBubbleWrapperRef.current.getBoundingClientRect().height,r=p.getPositionTop(t,n,a),i=o.default(window).height();if(r+a-window.pageYOffset>i){var s=r-a-2*b.top;s>0&&"bottom"!==t&&"top"!==t?(r=s,p.setState({addBottomRelativeClassToTargetedBubble:!0})):p.setState({addBottomRelativeClassToTargetedBubble:!1})}else p.setState({addBottomRelativeClassToTargetedBubble:!1});var l=p.targetedBubbleWrapperRef.current.getBoundingClientRect().width,u=p.getPositionLeft(t,n,l);p.setState({bubbleWrapperTopLeft:{top:r,left:u}})},p.setupAndPositionBackdrop=function(e){o.default(".targeted-bubble-backdrop__hole, .targeted-bubble-backdrop").remove();var t=o.default("<div></div>").addClass("targeted-bubble-backdrop__hole"),n=o.default("<div></div>").addClass("targeted-bubble-backdrop").append(t);p.getPromptSilo().append(n);var a=e.offset();return t.css({boxShadow:"rgba(0, 0, 0, 0.6) 0 0 0 30000px, inset 0 0 0 #111",width:e.outerWidth(),height:e.outerHeight(),position:p.isTargetFixed(e)?"fixed":"absolute",cursor:e.css("cursor")}),t.offset(a),{backdrop:n,backdropHole:t}},p.getButtonClassName=function(){return p.props.campaign.content.imageUrl?void 0:"small-margin"},p.getBubbleWrapperStyle=function(){return{zIndex:p.state.targetedBubbleWrapperZIndex,top:p.state.bubbleWrapperTopLeft?p.state.bubbleWrapperTopLeft.top:void 0,left:p.state.bubbleWrapperTopLeft?p.state.bubbleWrapperTopLeft.left:void 0}},p.getBubbleClassName=function(){var e=p.props.campaign.content.placement;return r.default({"chat-bubble":!0,"targeted-bubble":!0,"chat-bubble-top":null!==e&&"bottom"===e,"chat-bubble-right":null!==e&&"left"===e,"chat-bubble-left":null===e||"right"===e,"chat-bubble-bottom":null!==e&&"top"===e,"bottom-relative":p.state.addBottomRelativeClassToTargetedBubble,"with-x-icon":p.props.campaign.content.cancelWithXIcon,"without-x-icon":!p.props.campaign.content.cancelWithXIcon})},p.getPositionLeft=function(e,t,n){return"bottom"===e||"top"===e?t.left+Math.floor(t.width/2)-n+b.right:"left"===e?b.left-n+t.left:-b.left+t.width+t.left},p.getPositionTop=function(e,t,n){return"bottom"===e?t.top+t.height+b.bottom:"top"===e?t.top-(n+b.bottom):b.top+t.top+Math.floor(t.height/2)},p.onDismissIconClick=function(e){p.props.isPreview||(p.setState({isShown:!1}),p.state_manager.onDismiss(e))},p.renderDismissButton=function(){return p.props.campaign.content.cancelWithXIcon?a.default.createElement(s.DismissButton,{className:"close-button dismiss-x-icon",onDismiss:p.onDismissIconClick,"aria-label":d._("Close")},a.default.createElement(c.Sprite,{group:"web",name:"close_small",alt:d._("Close")})):null},i=n.__assign({},i,{addBottomRelativeClassToTargetedBubble:!1,bubbleWrapperTopLeft:void 0}),p.targetedBubbleWrapperRef=a.default.createRef(),p}return n.__extends(t,e),t.prototype.componentDidMount=function(){this.props.isPreview||this.waitForElementThenSetupAndPosition()},t.prototype.render=function(){var e=this.props.campaign.content,t=e.header,n=e.text,o=t?a.default.createElement("h2",{className:"title"},t):null;return a.default.createElement("div",{className:this.getBubbleWrapperClassName(),style:this.getBubbleWrapperStyle(),ref:this.targetedBubbleWrapperRef},a.default.createElement("div",{className:this.getBubbleClassName()},this.getTargetedBubbleImage(),o,a.default.createElement("span",{className:"targeted-bubble__text"},n),a.default.createElement(s.PromptButtons,{campaign:this.props.campaign,confirmIsPost:this.state_manager.shouldConfirmUsingPost(),confirmUrl:this.state_manager.buildConfirmEndpointURI(),onConfirm:this.onConfirm,onDismiss:this.onDismiss,confirmButtonClassName:this.getButtonClassName(),dismissButtonClassName:this.getButtonClassName()}),this.renderDismissButton(),a.default.createElement("div",{className:"chat-bubble-arrow-border"}),a.default.createElement("div",{className:"chat-bubble-arrow"})))},t.displayName="TargetedBubbleWithoutCSS",t})(i.PromptLocation);t.TargetedBubbleRendererWithoutCSS=m,t.TargetedBubble=p.requireCssWithComponent(m,["/static/css/upsell/targeted_bubble-vflmDnPqq.css","/static/css/chatbubble-vflAxqf1H.css"])});
//# sourceMappingURL=targeted_bubble.min.js-vflqo9ve_.map