define(["require","exports","tslib","react","modules/clean/react/file_viewer/unity/unity_utils"],function(t,i,e,n,o){"use strict";Object.defineProperty(i,"__esModule",{value:!0}),n=e.__importDefault(n);var s=(function(t){function i(){var i=null!==t&&t.apply(this,arguments)||this;return i.mounted=!1,i.state={unityInfo:null},i}return e.__extends(i,t),i.prototype.componentDidMount=function(){this.mounted=!0,o.UnityHelpers.isUnityEnabled()&&this.setupUnity()},i.prototype.componentWillUnmount=function(){this.mounted=!1,window.clearTimeout(this.disableCheck)},i.prototype.setupUnity=function(){var t=this;if(this.props.isUnityDisabled)return void this.disableUnity();o.UnityHelpers.getUnityFileInfo(this.props.file.ns_id,this.props.file.ns_path,this.props.user.id,function(i){t.mounted&&t.setState({unityInfo:i})},function(){t.disableUnity()}),this.disableCheck=window.setTimeout(function(){t.state.unityInfo||t.disableUnity()},1e4)},i.prototype.disableUnity=function(){this.setState({unityInfo:{local_path:null,resolved_local_path:null,can_open_directly:!1,is_locally_available:!1,is_infinite_placeholder:!1,open_application_identifier:null,open_application_name:null,path_is_dir:!1,error_message:null}})},i.prototype.render=function(){return this.props.render(this.state.unityInfo)},i})(n.default.Component);i.WithUnity=s});
//# sourceMappingURL=with_unity.min.js-vflU1WsFM.map