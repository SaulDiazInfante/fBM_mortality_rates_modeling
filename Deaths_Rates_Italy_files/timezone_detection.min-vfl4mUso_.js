define(["require","exports","tslib","modules/clean/viewer","modules/clean/ajax","modules/constants/time"],function(e,t,n,i,s,r){"use strict";function o(){if(i.Viewer.get_viewer().is_signed_in){var e=u();null!=r.AUTO_TIMEZONE_OFFSET&&r.AUTO_TIMEZONE_OFFSET===e||a(e)}}function u(){var e=new Date;e.setSeconds(0),e.setMilliseconds(0);var t=e.toUTCString(),n=new Date(t.substring(0,t.lastIndexOf(" ")));return(e.getTime()-n.getTime())/36e5}function a(e){s.SilentBackgroundRequest({url:"/set_timezone",data:{offset:e}})}Object.defineProperty(t,"__esModule",{value:!0}),s=n.__importStar(s),t.checkTimezone=o,t.getCurrentTimezone=u,t.update=a});
//# sourceMappingURL=timezone_detection.min.js-vflBZx9wO.map