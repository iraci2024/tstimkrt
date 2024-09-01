/*
 *  Copyright (c) 2014 The WebRTC project authors. All Rights Reserved.
 *
 *  Use of this source code is governed by a BSD-style license
 *  that can be found in the LICENSE file in the root of the source
 *  tree.
 */

/* global pako */

window.rtstats = {};

(function() {
  var pcs = [];

  window.addEventListener('pccreated', function(ev) {
    pcs.push(ev.detail);
  }, false);

  window.addEventListener('pcclosed', function(ev) {
    var index = -1;
    for (index = 0; index < pcs.length; index++) {
      if (pcs[index].pc === ev.detail.pc) {
        break;
      }
    }
    if (index > -1) {
      pcs.splice(index, 1);
    } else {
      console.log('Unknown peer connection closed?');
    }
  }, false);

  window.rtstats.activePCs = function() {
    return pcs;
  };
}());


!function(t){if("object"==typeof exports&&"undefined"!=typeof module)module.exports=t();else if("function"==typeof define&&define.amd)define([],t);else{("undefined"!=typeof window?window:"undefined"!=typeof global?global:"undefined"!=typeof self?self:this).pako=t()}}(function(){return function t(e,a,i){function n(s,o){if(!a[s]){if(!e[s]){var l="function"==typeof require&&require;if(!o&&l)return l(s,!0);if(r)return r(s,!0);var h=new Error("Cannot find module '"+s+"'");throw h.code="MODULE_NOT_FOUND",h}var d=a[s]={exports:{}};e[s][0].call(d.exports,function(t){var a=e[s][1][t];return n(a||t)},d,d.exports,t,e,a,i)}return a[s].exports}for(var r="function"==typeof require&&require,s=0;s<i.length;s++)n(i[s]);return n}({1:[function(t,e,a){"use strict";function i(t){if(!(this instanceof i))return new i(t);this.options=s.assign({level:_,method:c,chunkSize:16384,windowBits:15,memLevel:8,strategy:u,to:""},t||{});var e=this.options;e.raw&&e.windowBits>0?e.windowBits=-e.windowBits:e.gzip&&e.windowBits>0&&e.windowBits<16&&(e.windowBits+=16),this.err=0,this.msg="",this.ended=!1,this.chunks=[],this.strm=new h,this.strm.avail_out=0;var a=r.deflateInit2(this.strm,e.level,e.method,e.windowBits,e.memLevel,e.strategy);if(a!==f)throw new Error(l[a]);if(e.header&&r.deflateSetHeader(this.strm,e.header),e.dictionary){var n;if(n="string"==typeof e.dictionary?o.string2buf(e.dictionary):"[object ArrayBuffer]"===d.call(e.dictionary)?new Uint8Array(e.dictionary):e.dictionary,(a=r.deflateSetDictionary(this.strm,n))!==f)throw new Error(l[a]);this._dict_set=!0}}function n(t,e){var a=new i(e);if(a.push(t,!0),a.err)throw a.msg||l[a.err];return a.result}var r=t("./zlib/deflate"),s=t("./utils/common"),o=t("./utils/strings"),l=t("./zlib/messages"),h=t("./zlib/zstream"),d=Object.prototype.toString,f=0,_=-1,u=0,c=8;i.prototype.push=function(t,e){var a,i,n=this.strm,l=this.options.chunkSize;if(this.ended)return!1;i=e===~~e?e:!0===e?4:0,"string"==typeof t?n.input=o.string2buf(t):"[object ArrayBuffer]"===d.call(t)?n.input=new Uint8Array(t):n.input=t,n.next_in=0,n.avail_in=n.input.length;do{if(0===n.avail_out&&(n.output=new s.Buf8(l),n.next_out=0,n.avail_out=l),1!==(a=r.deflate(n,i))&&a!==f)return this.onEnd(a),this.ended=!0,!1;0!==n.avail_out&&(0!==n.avail_in||4!==i&&2!==i)||("string"===this.options.to?this.onData(o.buf2binstring(s.shrinkBuf(n.output,n.next_out))):this.onData(s.shrinkBuf(n.output,n.next_out)))}while((n.avail_in>0||0===n.avail_out)&&1!==a);return 4===i?(a=r.deflateEnd(this.strm),this.onEnd(a),this.ended=!0,a===f):2!==i||(this.onEnd(f),n.avail_out=0,!0)},i.prototype.onData=function(t){this.chunks.push(t)},i.prototype.onEnd=function(t){t===f&&("string"===this.options.to?this.result=this.chunks.join(""):this.result=s.flattenChunks(this.chunks)),this.chunks=[],this.err=t,this.msg=this.strm.msg},a.Deflate=i,a.deflate=n,a.deflateRaw=function(t,e){return e=e||{},e.raw=!0,n(t,e)},a.gzip=function(t,e){return e=e||{},e.gzip=!0,n(t,e)}},{"./utils/common":3,"./utils/strings":4,"./zlib/deflate":8,"./zlib/messages":13,"./zlib/zstream":15}],2:[function(t,e,a){"use strict";function i(t){if(!(this instanceof i))return new i(t);this.options=s.assign({chunkSize:16384,windowBits:0,to:""},t||{});var e=this.options;e.raw&&e.windowBits>=0&&e.windowBits<16&&(e.windowBits=-e.windowBits,0===e.windowBits&&(e.windowBits=-15)),!(e.windowBits>=0&&e.windowBits<16)||t&&t.windowBits||(e.windowBits+=32),e.windowBits>15&&e.windowBits<48&&0==(15&e.windowBits)&&(e.windowBits|=15),this.err=0,this.msg="",this.ended=!1,this.chunks=[],this.strm=new d,this.strm.avail_out=0;var a=r.inflateInit2(this.strm,e.windowBits);if(a!==l.Z_OK)throw new Error(h[a]);this.header=new f,r.inflateGetHeader(this.strm,this.header)}function n(t,e){var a=new i(e);if(a.push(t,!0),a.err)throw a.msg||h[a.err];return a.result}var r=t("./zlib/inflate"),s=t("./utils/common"),o=t("./utils/strings"),l=t("./zlib/constants"),h=t("./zlib/messages"),d=t("./zlib/zstream"),f=t("./zlib/gzheader"),_=Object.prototype.toString;i.prototype.push=function(t,e){var a,i,n,h,d,f,u=this.strm,c=this.options.chunkSize,b=this.options.dictionary,g=!1;if(this.ended)return!1;i=e===~~e?e:!0===e?l.Z_FINISH:l.Z_NO_FLUSH,"string"==typeof t?u.input=o.binstring2buf(t):"[object ArrayBuffer]"===_.call(t)?u.input=new Uint8Array(t):u.input=t,u.next_in=0,u.avail_in=u.input.length;do{if(0===u.avail_out&&(u.output=new s.Buf8(c),u.next_out=0,u.avail_out=c),(a=r.inflate(u,l.Z_NO_FLUSH))===l.Z_NEED_DICT&&b&&(f="string"==typeof b?o.string2buf(b):"[object ArrayBuffer]"===_.call(b)?new Uint8Array(b):b,a=r.inflateSetDictionary(this.strm,f)),a===l.Z_BUF_ERROR&&!0===g&&(a=l.Z_OK,g=!1),a!==l.Z_STREAM_END&&a!==l.Z_OK)return this.onEnd(a),this.ended=!0,!1;u.next_out&&(0!==u.avail_out&&a!==l.Z_STREAM_END&&(0!==u.avail_in||i!==l.Z_FINISH&&i!==l.Z_SYNC_FLUSH)||("string"===this.options.to?(n=o.utf8border(u.output,u.next_out),h=u.next_out-n,d=o.buf2string(u.output,n),u.next_out=h,u.avail_out=c-h,h&&s.arraySet(u.output,u.output,n,h,0),this.onData(d)):this.onData(s.shrinkBuf(u.output,u.next_out)))),0===u.avail_in&&0===u.avail_out&&(g=!0)}while((u.avail_in>0||0===u.avail_out)&&a!==l.Z_STREAM_END);return a===l.Z_STREAM_END&&(i=l.Z_FINISH),i===l.Z_FINISH?(a=r.inflateEnd(this.strm),this.onEnd(a),this.ended=!0,a===l.Z_OK):i!==l.Z_SYNC_FLUSH||(this.onEnd(l.Z_OK),u.avail_out=0,!0)},i.prototype.onData=function(t){this.chunks.push(t)},i.prototype.onEnd=function(t){t===l.Z_OK&&("string"===this.options.to?this.result=this.chunks.join(""):this.result=s.flattenChunks(this.chunks)),this.chunks=[],this.err=t,this.msg=this.strm.msg},a.Inflate=i,a.inflate=n,a.inflateRaw=function(t,e){return e=e||{},e.raw=!0,n(t,e)},a.ungzip=n},{"./utils/common":3,"./utils/strings":4,"./zlib/constants":6,"./zlib/gzheader":9,"./zlib/inflate":11,"./zlib/messages":13,"./zlib/zstream":15}],3:[function(t,e,a){"use strict";function i(t,e){return Object.prototype.hasOwnProperty.call(t,e)}var n="undefined"!=typeof Uint8Array&&"undefined"!=typeof Uint16Array&&"undefined"!=typeof Int32Array;a.assign=function(t){for(var e=Array.prototype.slice.call(arguments,1);e.length;){var a=e.shift();if(a){if("object"!=typeof a)throw new TypeError(a+"must be non-object");for(var n in a)i(a,n)&&(t[n]=a[n])}}return t},a.shrinkBuf=function(t,e){return t.length===e?t:t.subarray?t.subarray(0,e):(t.length=e,t)};var r={arraySet:function(t,e,a,i,n){if(e.subarray&&t.subarray)t.set(e.subarray(a,a+i),n);else for(var r=0;r<i;r++)t[n+r]=e[a+r]},flattenChunks:function(t){var e,a,i,n,r,s;for(i=0,e=0,a=t.length;e<a;e++)i+=t[e].length;for(s=new Uint8Array(i),n=0,e=0,a=t.length;e<a;e++)r=t[e],s.set(r,n),n+=r.length;return s}},s={arraySet:function(t,e,a,i,n){for(var r=0;r<i;r++)t[n+r]=e[a+r]},flattenChunks:function(t){return[].concat.apply([],t)}};a.setTyped=function(t){t?(a.Buf8=Uint8Array,a.Buf16=Uint16Array,a.Buf32=Int32Array,a.assign(a,r)):(a.Buf8=Array,a.Buf16=Array,a.Buf32=Array,a.assign(a,s))},a.setTyped(n)},{}],4:[function(t,e,a){"use strict";function i(t,e){if(e<65537&&(t.subarray&&s||!t.subarray&&r))return String.fromCharCode.apply(null,n.shrinkBuf(t,e));for(var a="",i=0;i<e;i++)a+=String.fromCharCode(t[i]);return a}var n=t("./common"),r=!0,s=!0;try{String.fromCharCode.apply(null,[0])}catch(t){r=!1}try{String.fromCharCode.apply(null,new Uint8Array(1))}catch(t){s=!1}for(var o=new n.Buf8(256),l=0;l<256;l++)o[l]=l>=252?6:l>=248?5:l>=240?4:l>=224?3:l>=192?2:1;o[254]=o[254]=1,a.string2buf=function(t){var e,a,i,r,s,o=t.length,l=0;for(r=0;r<o;r++)55296==(64512&(a=t.charCodeAt(r)))&&r+1<o&&56320==(64512&(i=t.charCodeAt(r+1)))&&(a=65536+(a-55296<<10)+(i-56320),r++),l+=a<128?1:a<2048?2:a<65536?3:4;for(e=new n.Buf8(l),s=0,r=0;s<l;r++)55296==(64512&(a=t.charCodeAt(r)))&&r+1<o&&56320==(64512&(i=t.charCodeAt(r+1)))&&(a=65536+(a-55296<<10)+(i-56320),r++),a<128?e[s++]=a:a<2048?(e[s++]=192|a>>>6,e[s++]=128|63&a):a<65536?(e[s++]=224|a>>>12,e[s++]=128|a>>>6&63,e[s++]=128|63&a):(e[s++]=240|a>>>18,e[s++]=128|a>>>12&63,e[s++]=128|a>>>6&63,e[s++]=128|63&a);return e},a.buf2binstring=function(t){return i(t,t.length)},a.binstring2buf=function(t){for(var e=new n.Buf8(t.length),a=0,i=e.length;a<i;a++)e[a]=t.charCodeAt(a);return e},a.buf2string=function(t,e){var a,n,r,s,l=e||t.length,h=new Array(2*l);for(n=0,a=0;a<l;)if((r=t[a++])<128)h[n++]=r;else if((s=o[r])>4)h[n++]=65533,a+=s-1;else{for(r&=2===s?31:3===s?15:7;s>1&&a<l;)r=r<<6|63&t[a++],s--;s>1?h[n++]=65533:r<65536?h[n++]=r:(r-=65536,h[n++]=55296|r>>10&1023,h[n++]=56320|1023&r)}return i(h,n)},a.utf8border=function(t,e){var a;for((e=e||t.length)>t.length&&(e=t.length),a=e-1;a>=0&&128==(192&t[a]);)a--;return a<0?e:0===a?e:a+o[t[a]]>e?a:e}},{"./common":3}],5:[function(t,e,a){"use strict";e.exports=function(t,e,a,i){for(var n=65535&t|0,r=t>>>16&65535|0,s=0;0!==a;){a-=s=a>2e3?2e3:a;do{r=r+(n=n+e[i++]|0)|0}while(--s);n%=65521,r%=65521}return n|r<<16|0}},{}],6:[function(t,e,a){"use strict";e.exports={Z_NO_FLUSH:0,Z_PARTIAL_FLUSH:1,Z_SYNC_FLUSH:2,Z_FULL_FLUSH:3,Z_FINISH:4,Z_BLOCK:5,Z_TREES:6,Z_OK:0,Z_STREAM_END:1,Z_NEED_DICT:2,Z_ERRNO:-1,Z_STREAM_ERROR:-2,Z_DATA_ERROR:-3,Z_BUF_ERROR:-5,Z_NO_COMPRESSION:0,Z_BEST_SPEED:1,Z_BEST_COMPRESSION:9,Z_DEFAULT_COMPRESSION:-1,Z_FILTERED:1,Z_HUFFMAN_ONLY:2,Z_RLE:3,Z_FIXED:4,Z_DEFAULT_STRATEGY:0,Z_BINARY:0,Z_TEXT:1,Z_UNKNOWN:2,Z_DEFLATED:8}},{}],7:[function(t,e,a){"use strict";var i=function(){for(var t,e=[],a=0;a<256;a++){t=a;for(var i=0;i<8;i++)t=1&t?3988292384^t>>>1:t>>>1;e[a]=t}return e}();e.exports=function(t,e,a,n){var r=i,s=n+a;t^=-1;for(var o=n;o<s;o++)t=t>>>8^r[255&(t^e[o])];return-1^t}},{}],8:[function(t,e,a){"use strict";function i(t,e){return t.msg=A[e],e}function n(t){return(t<<1)-(t>4?9:0)}function r(t){for(var e=t.length;--e>=0;)t[e]=0}function s(t){var e=t.state,a=e.pending;a>t.avail_out&&(a=t.avail_out),0!==a&&(z.arraySet(t.output,e.pending_buf,e.pending_out,a,t.next_out),t.next_out+=a,e.pending_out+=a,t.total_out+=a,t.avail_out-=a,e.pending-=a,0===e.pending&&(e.pending_out=0))}function o(t,e){B._tr_flush_block(t,t.block_start>=0?t.block_start:-1,t.strstart-t.block_start,e),t.block_start=t.strstart,s(t.strm)}function l(t,e){t.pending_buf[t.pending++]=e}function h(t,e){t.pending_buf[t.pending++]=e>>>8&255,t.pending_buf[t.pending++]=255&e}function d(t,e,a,i){var n=t.avail_in;return n>i&&(n=i),0===n?0:(t.avail_in-=n,z.arraySet(e,t.input,t.next_in,n,a),1===t.state.wrap?t.adler=S(t.adler,e,n,a):2===t.state.wrap&&(t.adler=E(t.adler,e,n,a)),t.next_in+=n,t.total_in+=n,n)}function f(t,e){var a,i,n=t.max_chain_length,r=t.strstart,s=t.prev_length,o=t.nice_match,l=t.strstart>t.w_size-it?t.strstart-(t.w_size-it):0,h=t.window,d=t.w_mask,f=t.prev,_=t.strstart+at,u=h[r+s-1],c=h[r+s];t.prev_length>=t.good_match&&(n>>=2),o>t.lookahead&&(o=t.lookahead);do{if(a=e,h[a+s]===c&&h[a+s-1]===u&&h[a]===h[r]&&h[++a]===h[r+1]){r+=2,a++;do{}while(h[++r]===h[++a]&&h[++r]===h[++a]&&h[++r]===h[++a]&&h[++r]===h[++a]&&h[++r]===h[++a]&&h[++r]===h[++a]&&h[++r]===h[++a]&&h[++r]===h[++a]&&r<_);if(i=at-(_-r),r=_-at,i>s){if(t.match_start=e,s=i,i>=o)break;u=h[r+s-1],c=h[r+s]}}}while((e=f[e&d])>l&&0!=--n);return s<=t.lookahead?s:t.lookahead}function _(t){var e,a,i,n,r,s=t.w_size;do{if(n=t.window_size-t.lookahead-t.strstart,t.strstart>=s+(s-it)){z.arraySet(t.window,t.window,s,s,0),t.match_start-=s,t.strstart-=s,t.block_start-=s,e=a=t.hash_size;do{i=t.head[--e],t.head[e]=i>=s?i-s:0}while(--a);e=a=s;do{i=t.prev[--e],t.prev[e]=i>=s?i-s:0}while(--a);n+=s}if(0===t.strm.avail_in)break;if(a=d(t.strm,t.window,t.strstart+t.lookahead,n),t.lookahead+=a,t.lookahead+t.insert>=et)for(r=t.strstart-t.insert,t.ins_h=t.window[r],t.ins_h=(t.ins_h<<t.hash_shift^t.window[r+1])&t.hash_mask;t.insert&&(t.ins_h=(t.ins_h<<t.hash_shift^t.window[r+et-1])&t.hash_mask,t.prev[r&t.w_mask]=t.head[t.ins_h],t.head[t.ins_h]=r,r++,t.insert--,!(t.lookahead+t.insert<et)););}while(t.lookahead<it&&0!==t.strm.avail_in)}function u(t,e){for(var a,i;;){if(t.lookahead<it){if(_(t),t.lookahead<it&&e===Z)return _t;if(0===t.lookahead)break}if(a=0,t.lookahead>=et&&(t.ins_h=(t.ins_h<<t.hash_shift^t.window[t.strstart+et-1])&t.hash_mask,a=t.prev[t.strstart&t.w_mask]=t.head[t.ins_h],t.head[t.ins_h]=t.strstart),0!==a&&t.strstart-a<=t.w_size-it&&(t.match_length=f(t,a)),t.match_length>=et)if(i=B._tr_tally(t,t.strstart-t.match_start,t.match_length-et),t.lookahead-=t.match_length,t.match_length<=t.max_lazy_match&&t.lookahead>=et){t.match_length--;do{t.strstart++,t.ins_h=(t.ins_h<<t.hash_shift^t.window[t.strstart+et-1])&t.hash_mask,a=t.prev[t.strstart&t.w_mask]=t.head[t.ins_h],t.head[t.ins_h]=t.strstart}while(0!=--t.match_length);t.strstart++}else t.strstart+=t.match_length,t.match_length=0,t.ins_h=t.window[t.strstart],t.ins_h=(t.ins_h<<t.hash_shift^t.window[t.strstart+1])&t.hash_mask;else i=B._tr_tally(t,0,t.window[t.strstart]),t.lookahead--,t.strstart++;if(i&&(o(t,!1),0===t.strm.avail_out))return _t}return t.insert=t.strstart<et-1?t.strstart:et-1,e===N?(o(t,!0),0===t.strm.avail_out?ct:bt):t.last_lit&&(o(t,!1),0===t.strm.avail_out)?_t:ut}function c(t,e){for(var a,i,n;;){if(t.lookahead<it){if(_(t),t.lookahead<it&&e===Z)return _t;if(0===t.lookahead)break}if(a=0,t.lookahead>=et&&(t.ins_h=(t.ins_h<<t.hash_shift^t.window[t.strstart+et-1])&t.hash_mask,a=t.prev[t.strstart&t.w_mask]=t.head[t.ins_h],t.head[t.ins_h]=t.strstart),t.prev_length=t.match_length,t.prev_match=t.match_start,t.match_length=et-1,0!==a&&t.prev_length<t.max_lazy_match&&t.strstart-a<=t.w_size-it&&(t.match_length=f(t,a),t.match_length<=5&&(t.strategy===H||t.match_length===et&&t.strstart-t.match_start>4096)&&(t.match_length=et-1)),t.prev_length>=et&&t.match_length<=t.prev_length){n=t.strstart+t.lookahead-et,i=B._tr_tally(t,t.strstart-1-t.prev_match,t.prev_length-et),t.lookahead-=t.prev_length-1,t.prev_length-=2;do{++t.strstart<=n&&(t.ins_h=(t.ins_h<<t.hash_shift^t.window[t.strstart+et-1])&t.hash_mask,a=t.prev[t.strstart&t.w_mask]=t.head[t.ins_h],t.head[t.ins_h]=t.strstart)}while(0!=--t.prev_length);if(t.match_available=0,t.match_length=et-1,t.strstart++,i&&(o(t,!1),0===t.strm.avail_out))return _t}else if(t.match_available){if((i=B._tr_tally(t,0,t.window[t.strstart-1]))&&o(t,!1),t.strstart++,t.lookahead--,0===t.strm.avail_out)return _t}else t.match_available=1,t.strstart++,t.lookahead--}return t.match_available&&(i=B._tr_tally(t,0,t.window[t.strstart-1]),t.match_available=0),t.insert=t.strstart<et-1?t.strstart:et-1,e===N?(o(t,!0),0===t.strm.avail_out?ct:bt):t.last_lit&&(o(t,!1),0===t.strm.avail_out)?_t:ut}function b(t,e){for(var a,i,n,r,s=t.window;;){if(t.lookahead<=at){if(_(t),t.lookahead<=at&&e===Z)return _t;if(0===t.lookahead)break}if(t.match_length=0,t.lookahead>=et&&t.strstart>0&&(n=t.strstart-1,(i=s[n])===s[++n]&&i===s[++n]&&i===s[++n])){r=t.strstart+at;do{}while(i===s[++n]&&i===s[++n]&&i===s[++n]&&i===s[++n]&&i===s[++n]&&i===s[++n]&&i===s[++n]&&i===s[++n]&&n<r);t.match_length=at-(r-n),t.match_length>t.lookahead&&(t.match_length=t.lookahead)}if(t.match_length>=et?(a=B._tr_tally(t,1,t.match_length-et),t.lookahead-=t.match_length,t.strstart+=t.match_length,t.match_length=0):(a=B._tr_tally(t,0,t.window[t.strstart]),t.lookahead--,t.strstart++),a&&(o(t,!1),0===t.strm.avail_out))return _t}return t.insert=0,e===N?(o(t,!0),0===t.strm.avail_out?ct:bt):t.last_lit&&(o(t,!1),0===t.strm.avail_out)?_t:ut}function g(t,e){for(var a;;){if(0===t.lookahead&&(_(t),0===t.lookahead)){if(e===Z)return _t;break}if(t.match_length=0,a=B._tr_tally(t,0,t.window[t.strstart]),t.lookahead--,t.strstart++,a&&(o(t,!1),0===t.strm.avail_out))return _t}return t.insert=0,e===N?(o(t,!0),0===t.strm.avail_out?ct:bt):t.last_lit&&(o(t,!1),0===t.strm.avail_out)?_t:ut}function m(t,e,a,i,n){this.good_length=t,this.max_lazy=e,this.nice_length=a,this.max_chain=i,this.func=n}function w(t){t.window_size=2*t.w_size,r(t.head),t.max_lazy_match=x[t.level].max_lazy,t.good_match=x[t.level].good_length,t.nice_match=x[t.level].nice_length,t.max_chain_length=x[t.level].max_chain,t.strstart=0,t.block_start=0,t.lookahead=0,t.insert=0,t.match_length=t.prev_length=et-1,t.match_available=0,t.ins_h=0}function p(){this.strm=null,this.status=0,this.pending_buf=null,this.pending_buf_size=0,this.pending_out=0,this.pending=0,this.wrap=0,this.gzhead=null,this.gzindex=0,this.method=q,this.last_flush=-1,this.w_size=0,this.w_bits=0,this.w_mask=0,this.window=null,this.window_size=0,this.prev=null,this.head=null,this.ins_h=0,this.hash_size=0,this.hash_bits=0,this.hash_mask=0,this.hash_shift=0,this.block_start=0,this.match_length=0,this.prev_match=0,this.match_available=0,this.strstart=0,this.match_start=0,this.lookahead=0,this.prev_length=0,this.max_chain_length=0,this.max_lazy_match=0,this.level=0,this.strategy=0,this.good_match=0,this.nice_match=0,this.dyn_ltree=new z.Buf16(2*$),this.dyn_dtree=new z.Buf16(2*(2*Q+1)),this.bl_tree=new z.Buf16(2*(2*V+1)),r(this.dyn_ltree),r(this.dyn_dtree),r(this.bl_tree),this.l_desc=null,this.d_desc=null,this.bl_desc=null,this.bl_count=new z.Buf16(tt+1),this.heap=new z.Buf16(2*J+1),r(this.heap),this.heap_len=0,this.heap_max=0,this.depth=new z.Buf16(2*J+1),r(this.depth),this.l_buf=0,this.lit_bufsize=0,this.last_lit=0,this.d_buf=0,this.opt_len=0,this.static_len=0,this.matches=0,this.insert=0,this.bi_buf=0,this.bi_valid=0}function v(t){var e;return t&&t.state?(t.total_in=t.total_out=0,t.data_type=Y,e=t.state,e.pending=0,e.pending_out=0,e.wrap<0&&(e.wrap=-e.wrap),e.status=e.wrap?rt:dt,t.adler=2===e.wrap?0:1,e.last_flush=Z,B._tr_init(e),D):i(t,U)}function k(t){var e=v(t);return e===D&&w(t.state),e}function y(t,e,a,n,r,s){if(!t)return U;var o=1;if(e===L&&(e=6),n<0?(o=0,n=-n):n>15&&(o=2,n-=16),r<1||r>G||a!==q||n<8||n>15||e<0||e>9||s<0||s>M)return i(t,U);8===n&&(n=9);var l=new p;return t.state=l,l.strm=t,l.wrap=o,l.gzhead=null,l.w_bits=n,l.w_size=1<<l.w_bits,l.w_mask=l.w_size-1,l.hash_bits=r+7,l.hash_size=1<<l.hash_bits,l.hash_mask=l.hash_size-1,l.hash_shift=~~((l.hash_bits+et-1)/et),l.window=new z.Buf8(2*l.w_size),l.head=new z.Buf16(l.hash_size),l.prev=new z.Buf16(l.w_size),l.lit_bufsize=1<<r+6,l.pending_buf_size=4*l.lit_bufsize,l.pending_buf=new z.Buf8(l.pending_buf_size),l.d_buf=1*l.lit_bufsize,l.l_buf=3*l.lit_bufsize,l.level=e,l.strategy=s,l.method=a,k(t)}var x,z=t("../utils/common"),B=t("./trees"),S=t("./adler32"),E=t("./crc32"),A=t("./messages"),Z=0,R=1,C=3,N=4,O=5,D=0,I=1,U=-2,T=-3,F=-5,L=-1,H=1,j=2,K=3,M=4,P=0,Y=2,q=8,G=9,X=15,W=8,J=286,Q=30,V=19,$=2*J+1,tt=15,et=3,at=258,it=at+et+1,nt=32,rt=42,st=69,ot=73,lt=91,ht=103,dt=113,ft=666,_t=1,ut=2,ct=3,bt=4,gt=3;x=[new m(0,0,0,0,function(t,e){var a=65535;for(a>t.pending_buf_size-5&&(a=t.pending_buf_size-5);;){if(t.lookahead<=1){if(_(t),0===t.lookahead&&e===Z)return _t;if(0===t.lookahead)break}t.strstart+=t.lookahead,t.lookahead=0;var i=t.block_start+a;if((0===t.strstart||t.strstart>=i)&&(t.lookahead=t.strstart-i,t.strstart=i,o(t,!1),0===t.strm.avail_out))return _t;if(t.strstart-t.block_start>=t.w_size-it&&(o(t,!1),0===t.strm.avail_out))return _t}return t.insert=0,e===N?(o(t,!0),0===t.strm.avail_out?ct:bt):(t.strstart>t.block_start&&(o(t,!1),t.strm.avail_out),_t)}),new m(4,4,8,4,u),new m(4,5,16,8,u),new m(4,6,32,32,u),new m(4,4,16,16,c),new m(8,16,32,32,c),new m(8,16,128,128,c),new m(8,32,128,256,c),new m(32,128,258,1024,c),new m(32,258,258,4096,c)],a.deflateInit=function(t,e){return y(t,e,q,X,W,P)},a.deflateInit2=y,a.deflateReset=k,a.deflateResetKeep=v,a.deflateSetHeader=function(t,e){return t&&t.state?2!==t.state.wrap?U:(t.state.gzhead=e,D):U},a.deflate=function(t,e){var a,o,d,f;if(!t||!t.state||e>O||e<0)return t?i(t,U):U;if(o=t.state,!t.output||!t.input&&0!==t.avail_in||o.status===ft&&e!==N)return i(t,0===t.avail_out?F:U);if(o.strm=t,a=o.last_flush,o.last_flush=e,o.status===rt)if(2===o.wrap)t.adler=0,l(o,31),l(o,139),l(o,8),o.gzhead?(l(o,(o.gzhead.text?1:0)+(o.gzhead.hcrc?2:0)+(o.gzhead.extra?4:0)+(o.gzhead.name?8:0)+(o.gzhead.comment?16:0)),l(o,255&o.gzhead.time),l(o,o.gzhead.time>>8&255),l(o,o.gzhead.time>>16&255),l(o,o.gzhead.time>>24&255),l(o,9===o.level?2:o.strategy>=j||o.level<2?4:0),l(o,255&o.gzhead.os),o.gzhead.extra&&o.gzhead.extra.length&&(l(o,255&o.gzhead.extra.length),l(o,o.gzhead.extra.length>>8&255)),o.gzhead.hcrc&&(t.adler=E(t.adler,o.pending_buf,o.pending,0)),o.gzindex=0,o.status=st):(l(o,0),l(o,0),l(o,0),l(o,0),l(o,0),l(o,9===o.level?2:o.strategy>=j||o.level<2?4:0),l(o,gt),o.status=dt);else{var _=q+(o.w_bits-8<<4)<<8;_|=(o.strategy>=j||o.level<2?0:o.level<6?1:6===o.level?2:3)<<6,0!==o.strstart&&(_|=nt),_+=31-_%31,o.status=dt,h(o,_),0!==o.strstart&&(h(o,t.adler>>>16),h(o,65535&t.adler)),t.adler=1}if(o.status===st)if(o.gzhead.extra){for(d=o.pending;o.gzindex<(65535&o.gzhead.extra.length)&&(o.pending!==o.pending_buf_size||(o.gzhead.hcrc&&o.pending>d&&(t.adler=E(t.adler,o.pending_buf,o.pending-d,d)),s(t),d=o.pending,o.pending!==o.pending_buf_size));)l(o,255&o.gzhead.extra[o.gzindex]),o.gzindex++;o.gzhead.hcrc&&o.pending>d&&(t.adler=E(t.adler,o.pending_buf,o.pending-d,d)),o.gzindex===o.gzhead.extra.length&&(o.gzindex=0,o.status=ot)}else o.status=ot;if(o.status===ot)if(o.gzhead.name){d=o.pending;do{if(o.pending===o.pending_buf_size&&(o.gzhead.hcrc&&o.pending>d&&(t.adler=E(t.adler,o.pending_buf,o.pending-d,d)),s(t),d=o.pending,o.pending===o.pending_buf_size)){f=1;break}f=o.gzindex<o.gzhead.name.length?255&o.gzhead.name.charCodeAt(o.gzindex++):0,l(o,f)}while(0!==f);o.gzhead.hcrc&&o.pending>d&&(t.adler=E(t.adler,o.pending_buf,o.pending-d,d)),0===f&&(o.gzindex=0,o.status=lt)}else o.status=lt;if(o.status===lt)if(o.gzhead.comment){d=o.pending;do{if(o.pending===o.pending_buf_size&&(o.gzhead.hcrc&&o.pending>d&&(t.adler=E(t.adler,o.pending_buf,o.pending-d,d)),s(t),d=o.pending,o.pending===o.pending_buf_size)){f=1;break}f=o.gzindex<o.gzhead.comment.length?255&o.gzhead.comment.charCodeAt(o.gzindex++):0,l(o,f)}while(0!==f);o.gzhead.hcrc&&o.pending>d&&(t.adler=E(t.adler,o.pending_buf,o.pending-d,d)),0===f&&(o.status=ht)}else o.status=ht;if(o.status===ht&&(o.gzhead.hcrc?(o.pending+2>o.pending_buf_size&&s(t),o.pending+2<=o.pending_buf_size&&(l(o,255&t.adler),l(o,t.adler>>8&255),t.adler=0,o.status=dt)):o.status=dt),0!==o.pending){if(s(t),0===t.avail_out)return o.last_flush=-1,D}else if(0===t.avail_in&&n(e)<=n(a)&&e!==N)return i(t,F);if(o.status===ft&&0!==t.avail_in)return i(t,F);if(0!==t.avail_in||0!==o.lookahead||e!==Z&&o.status!==ft){var u=o.strategy===j?g(o,e):o.strategy===K?b(o,e):x[o.level].func(o,e);if(u!==ct&&u!==bt||(o.status=ft),u===_t||u===ct)return 0===t.avail_out&&(o.last_flush=-1),D;if(u===ut&&(e===R?B._tr_align(o):e!==O&&(B._tr_stored_block(o,0,0,!1),e===C&&(r(o.head),0===o.lookahead&&(o.strstart=0,o.block_start=0,o.insert=0))),s(t),0===t.avail_out))return o.last_flush=-1,D}return e!==N?D:o.wrap<=0?I:(2===o.wrap?(l(o,255&t.adler),l(o,t.adler>>8&255),l(o,t.adler>>16&255),l(o,t.adler>>24&255),l(o,255&t.total_in),l(o,t.total_in>>8&255),l(o,t.total_in>>16&255),l(o,t.total_in>>24&255)):(h(o,t.adler>>>16),h(o,65535&t.adler)),s(t),o.wrap>0&&(o.wrap=-o.wrap),0!==o.pending?D:I)},a.deflateEnd=function(t){var e;return t&&t.state?(e=t.state.status)!==rt&&e!==st&&e!==ot&&e!==lt&&e!==ht&&e!==dt&&e!==ft?i(t,U):(t.state=null,e===dt?i(t,T):D):U},a.deflateSetDictionary=function(t,e){var a,i,n,s,o,l,h,d,f=e.length;if(!t||!t.state)return U;if(a=t.state,2===(s=a.wrap)||1===s&&a.status!==rt||a.lookahead)return U;for(1===s&&(t.adler=S(t.adler,e,f,0)),a.wrap=0,f>=a.w_size&&(0===s&&(r(a.head),a.strstart=0,a.block_start=0,a.insert=0),d=new z.Buf8(a.w_size),z.arraySet(d,e,f-a.w_size,a.w_size,0),e=d,f=a.w_size),o=t.avail_in,l=t.next_in,h=t.input,t.avail_in=f,t.next_in=0,t.input=e,_(a);a.lookahead>=et;){i=a.strstart,n=a.lookahead-(et-1);do{a.ins_h=(a.ins_h<<a.hash_shift^a.window[i+et-1])&a.hash_mask,a.prev[i&a.w_mask]=a.head[a.ins_h],a.head[a.ins_h]=i,i++}while(--n);a.strstart=i,a.lookahead=et-1,_(a)}return a.strstart+=a.lookahead,a.block_start=a.strstart,a.insert=a.lookahead,a.lookahead=0,a.match_length=a.prev_length=et-1,a.match_available=0,t.next_in=l,t.input=h,t.avail_in=o,a.wrap=s,D},a.deflateInfo="pako deflate (from Nodeca project)"},{"../utils/common":3,"./adler32":5,"./crc32":7,"./messages":13,"./trees":14}],9:[function(t,e,a){"use strict";e.exports=function(){this.text=0,this.time=0,this.xflags=0,this.os=0,this.extra=null,this.extra_len=0,this.name="",this.comment="",this.hcrc=0,this.done=!1}},{}],10:[function(t,e,a){"use strict";e.exports=function(t,e){var a,i,n,r,s,o,l,h,d,f,_,u,c,b,g,m,w,p,v,k,y,x,z,B,S;a=t.state,i=t.next_in,B=t.input,n=i+(t.avail_in-5),r=t.next_out,S=t.output,s=r-(e-t.avail_out),o=r+(t.avail_out-257),l=a.dmax,h=a.wsize,d=a.whave,f=a.wnext,_=a.window,u=a.hold,c=a.bits,b=a.lencode,g=a.distcode,m=(1<<a.lenbits)-1,w=(1<<a.distbits)-1;t:do{c<15&&(u+=B[i++]<<c,c+=8,u+=B[i++]<<c,c+=8),p=b[u&m];e:for(;;){if(v=p>>>24,u>>>=v,c-=v,0===(v=p>>>16&255))S[r++]=65535&p;else{if(!(16&v)){if(0==(64&v)){p=b[(65535&p)+(u&(1<<v)-1)];continue e}if(32&v){a.mode=12;break t}t.msg="invalid literal/length code",a.mode=30;break t}k=65535&p,(v&=15)&&(c<v&&(u+=B[i++]<<c,c+=8),k+=u&(1<<v)-1,u>>>=v,c-=v),c<15&&(u+=B[i++]<<c,c+=8,u+=B[i++]<<c,c+=8),p=g[u&w];a:for(;;){if(v=p>>>24,u>>>=v,c-=v,!(16&(v=p>>>16&255))){if(0==(64&v)){p=g[(65535&p)+(u&(1<<v)-1)];continue a}t.msg="invalid distance code",a.mode=30;break t}if(y=65535&p,v&=15,c<v&&(u+=B[i++]<<c,(c+=8)<v&&(u+=B[i++]<<c,c+=8)),(y+=u&(1<<v)-1)>l){t.msg="invalid distance too far back",a.mode=30;break t}if(u>>>=v,c-=v,v=r-s,y>v){if((v=y-v)>d&&a.sane){t.msg="invalid distance too far back",a.mode=30;break t}if(x=0,z=_,0===f){if(x+=h-v,v<k){k-=v;do{S[r++]=_[x++]}while(--v);x=r-y,z=S}}else if(f<v){if(x+=h+f-v,(v-=f)<k){k-=v;do{S[r++]=_[x++]}while(--v);if(x=0,f<k){k-=v=f;do{S[r++]=_[x++]}while(--v);x=r-y,z=S}}}else if(x+=f-v,v<k){k-=v;do{S[r++]=_[x++]}while(--v);x=r-y,z=S}for(;k>2;)S[r++]=z[x++],S[r++]=z[x++],S[r++]=z[x++],k-=3;k&&(S[r++]=z[x++],k>1&&(S[r++]=z[x++]))}else{x=r-y;do{S[r++]=S[x++],S[r++]=S[x++],S[r++]=S[x++],k-=3}while(k>2);k&&(S[r++]=S[x++],k>1&&(S[r++]=S[x++]))}break}}break}}while(i<n&&r<o);i-=k=c>>3,u&=(1<<(c-=k<<3))-1,t.next_in=i,t.next_out=r,t.avail_in=i<n?n-i+5:5-(i-n),t.avail_out=r<o?o-r+257:257-(r-o),a.hold=u,a.bits=c}},{}],11:[function(t,e,a){"use strict";function i(t){return(t>>>24&255)+(t>>>8&65280)+((65280&t)<<8)+((255&t)<<24)}function n(){this.mode=0,this.last=!1,this.wrap=0,this.havedict=!1,this.flags=0,this.dmax=0,this.check=0,this.total=0,this.head=null,this.wbits=0,this.wsize=0,this.whave=0,this.wnext=0,this.window=null,this.hold=0,this.bits=0,this.length=0,this.offset=0,this.extra=0,this.lencode=null,this.distcode=null,this.lenbits=0,this.distbits=0,this.ncode=0,this.nlen=0,this.ndist=0,this.have=0,this.next=null,this.lens=new u.Buf16(320),this.work=new u.Buf16(288),this.lendyn=null,this.distdyn=null,this.sane=0,this.back=0,this.was=0}function r(t){var e;return t&&t.state?(e=t.state,t.total_in=t.total_out=e.total=0,t.msg="",e.wrap&&(t.adler=1&e.wrap),e.mode=N,e.last=0,e.havedict=0,e.dmax=32768,e.head=null,e.hold=0,e.bits=0,e.lencode=e.lendyn=new u.Buf32(dt),e.distcode=e.distdyn=new u.Buf32(ft),e.sane=1,e.back=-1,z):E}function s(t){var e;return t&&t.state?(e=t.state,e.wsize=0,e.whave=0,e.wnext=0,r(t)):E}function o(t,e){var a,i;return t&&t.state?(i=t.state,e<0?(a=0,e=-e):(a=1+(e>>4),e<48&&(e&=15)),e&&(e<8||e>15)?E:(null!==i.window&&i.wbits!==e&&(i.window=null),i.wrap=a,i.wbits=e,s(t))):E}function l(t,e){var a,i;return t?(i=new n,t.state=i,i.window=null,(a=o(t,e))!==z&&(t.state=null),a):E}function h(t){if(ut){var e;for(f=new u.Buf32(512),_=new u.Buf32(32),e=0;e<144;)t.lens[e++]=8;for(;e<256;)t.lens[e++]=9;for(;e<280;)t.lens[e++]=7;for(;e<288;)t.lens[e++]=8;for(m(p,t.lens,0,288,f,0,t.work,{bits:9}),e=0;e<32;)t.lens[e++]=5;m(v,t.lens,0,32,_,0,t.work,{bits:5}),ut=!1}t.lencode=f,t.lenbits=9,t.distcode=_,t.distbits=5}function d(t,e,a,i){var n,r=t.state;return null===r.window&&(r.wsize=1<<r.wbits,r.wnext=0,r.whave=0,r.window=new u.Buf8(r.wsize)),i>=r.wsize?(u.arraySet(r.window,e,a-r.wsize,r.wsize,0),r.wnext=0,r.whave=r.wsize):((n=r.wsize-r.wnext)>i&&(n=i),u.arraySet(r.window,e,a-i,n,r.wnext),(i-=n)?(u.arraySet(r.window,e,a-i,i,0),r.wnext=i,r.whave=r.wsize):(r.wnext+=n,r.wnext===r.wsize&&(r.wnext=0),r.whave<r.wsize&&(r.whave+=n))),0}var f,_,u=t("../utils/common"),c=t("./adler32"),b=t("./crc32"),g=t("./inffast"),m=t("./inftrees"),w=0,p=1,v=2,k=4,y=5,x=6,z=0,B=1,S=2,E=-2,A=-3,Z=-4,R=-5,C=8,N=1,O=2,D=3,I=4,U=5,T=6,F=7,L=8,H=9,j=10,K=11,M=12,P=13,Y=14,q=15,G=16,X=17,W=18,J=19,Q=20,V=21,$=22,tt=23,et=24,at=25,it=26,nt=27,rt=28,st=29,ot=30,lt=31,ht=32,dt=852,ft=592,_t=15,ut=!0;a.inflateReset=s,a.inflateReset2=o,a.inflateResetKeep=r,a.inflateInit=function(t){return l(t,_t)},a.inflateInit2=l,a.inflate=function(t,e){var a,n,r,s,o,l,f,_,dt,ft,_t,ut,ct,bt,gt,mt,wt,pt,vt,kt,yt,xt,zt,Bt,St=0,Et=new u.Buf8(4),At=[16,17,18,0,8,7,9,6,10,5,11,4,12,3,13,2,14,1,15];if(!t||!t.state||!t.output||!t.input&&0!==t.avail_in)return E;(a=t.state).mode===M&&(a.mode=P),o=t.next_out,r=t.output,f=t.avail_out,s=t.next_in,n=t.input,l=t.avail_in,_=a.hold,dt=a.bits,ft=l,_t=f,xt=z;t:for(;;)switch(a.mode){case N:if(0===a.wrap){a.mode=P;break}for(;dt<16;){if(0===l)break t;l--,_+=n[s++]<<dt,dt+=8}if(2&a.wrap&&35615===_){a.check=0,Et[0]=255&_,Et[1]=_>>>8&255,a.check=b(a.check,Et,2,0),_=0,dt=0,a.mode=O;break}if(a.flags=0,a.head&&(a.head.done=!1),!(1&a.wrap)||(((255&_)<<8)+(_>>8))%31){t.msg="incorrect header check",a.mode=ot;break}if((15&_)!==C){t.msg="unknown compression method",a.mode=ot;break}if(_>>>=4,dt-=4,yt=8+(15&_),0===a.wbits)a.wbits=yt;else if(yt>a.wbits){t.msg="invalid window size",a.mode=ot;break}a.dmax=1<<yt,t.adler=a.check=1,a.mode=512&_?j:M,_=0,dt=0;break;case O:for(;dt<16;){if(0===l)break t;l--,_+=n[s++]<<dt,dt+=8}if(a.flags=_,(255&a.flags)!==C){t.msg="unknown compression method",a.mode=ot;break}if(57344&a.flags){t.msg="unknown header flags set",a.mode=ot;break}a.head&&(a.head.text=_>>8&1),512&a.flags&&(Et[0]=255&_,Et[1]=_>>>8&255,a.check=b(a.check,Et,2,0)),_=0,dt=0,a.mode=D;case D:for(;dt<32;){if(0===l)break t;l--,_+=n[s++]<<dt,dt+=8}a.head&&(a.head.time=_),512&a.flags&&(Et[0]=255&_,Et[1]=_>>>8&255,Et[2]=_>>>16&255,Et[3]=_>>>24&255,a.check=b(a.check,Et,4,0)),_=0,dt=0,a.mode=I;case I:for(;dt<16;){if(0===l)break t;l--,_+=n[s++]<<dt,dt+=8}a.head&&(a.head.xflags=255&_,a.head.os=_>>8),512&a.flags&&(Et[0]=255&_,Et[1]=_>>>8&255,a.check=b(a.check,Et,2,0)),_=0,dt=0,a.mode=U;case U:if(1024&a.flags){for(;dt<16;){if(0===l)break t;l--,_+=n[s++]<<dt,dt+=8}a.length=_,a.head&&(a.head.extra_len=_),512&a.flags&&(Et[0]=255&_,Et[1]=_>>>8&255,a.check=b(a.check,Et,2,0)),_=0,dt=0}else a.head&&(a.head.extra=null);a.mode=T;case T:if(1024&a.flags&&((ut=a.length)>l&&(ut=l),ut&&(a.head&&(yt=a.head.extra_len-a.length,a.head.extra||(a.head.extra=new Array(a.head.extra_len)),u.arraySet(a.head.extra,n,s,ut,yt)),512&a.flags&&(a.check=b(a.check,n,ut,s)),l-=ut,s+=ut,a.length-=ut),a.length))break t;a.length=0,a.mode=F;case F:if(2048&a.flags){if(0===l)break t;ut=0;do{yt=n[s+ut++],a.head&&yt&&a.length<65536&&(a.head.name+=String.fromCharCode(yt))}while(yt&&ut<l);if(512&a.flags&&(a.check=b(a.check,n,ut,s)),l-=ut,s+=ut,yt)break t}else a.head&&(a.head.name=null);a.length=0,a.mode=L;case L:if(4096&a.flags){if(0===l)break t;ut=0;do{yt=n[s+ut++],a.head&&yt&&a.length<65536&&(a.head.comment+=String.fromCharCode(yt))}while(yt&&ut<l);if(512&a.flags&&(a.check=b(a.check,n,ut,s)),l-=ut,s+=ut,yt)break t}else a.head&&(a.head.comment=null);a.mode=H;case H:if(512&a.flags){for(;dt<16;){if(0===l)break t;l--,_+=n[s++]<<dt,dt+=8}if(_!==(65535&a.check)){t.msg="header crc mismatch",a.mode=ot;break}_=0,dt=0}a.head&&(a.head.hcrc=a.flags>>9&1,a.head.done=!0),t.adler=a.check=0,a.mode=M;break;case j:for(;dt<32;){if(0===l)break t;l--,_+=n[s++]<<dt,dt+=8}t.adler=a.check=i(_),_=0,dt=0,a.mode=K;case K:if(0===a.havedict)return t.next_out=o,t.avail_out=f,t.next_in=s,t.avail_in=l,a.hold=_,a.bits=dt,S;t.adler=a.check=1,a.mode=M;case M:if(e===y||e===x)break t;case P:if(a.last){_>>>=7&dt,dt-=7&dt,a.mode=nt;break}for(;dt<3;){if(0===l)break t;l--,_+=n[s++]<<dt,dt+=8}switch(a.last=1&_,_>>>=1,dt-=1,3&_){case 0:a.mode=Y;break;case 1:if(h(a),a.mode=Q,e===x){_>>>=2,dt-=2;break t}break;case 2:a.mode=X;break;case 3:t.msg="invalid block type",a.mode=ot}_>>>=2,dt-=2;break;case Y:for(_>>>=7&dt,dt-=7&dt;dt<32;){if(0===l)break t;l--,_+=n[s++]<<dt,dt+=8}if((65535&_)!=(_>>>16^65535)){t.msg="invalid stored block lengths",a.mode=ot;break}if(a.length=65535&_,_=0,dt=0,a.mode=q,e===x)break t;case q:a.mode=G;case G:if(ut=a.length){if(ut>l&&(ut=l),ut>f&&(ut=f),0===ut)break t;u.arraySet(r,n,s,ut,o),l-=ut,s+=ut,f-=ut,o+=ut,a.length-=ut;break}a.mode=M;break;case X:for(;dt<14;){if(0===l)break t;l--,_+=n[s++]<<dt,dt+=8}if(a.nlen=257+(31&_),_>>>=5,dt-=5,a.ndist=1+(31&_),_>>>=5,dt-=5,a.ncode=4+(15&_),_>>>=4,dt-=4,a.nlen>286||a.ndist>30){t.msg="too many length or distance symbols",a.mode=ot;break}a.have=0,a.mode=W;case W:for(;a.have<a.ncode;){for(;dt<3;){if(0===l)break t;l--,_+=n[s++]<<dt,dt+=8}a.lens[At[a.have++]]=7&_,_>>>=3,dt-=3}for(;a.have<19;)a.lens[At[a.have++]]=0;if(a.lencode=a.lendyn,a.lenbits=7,zt={bits:a.lenbits},xt=m(w,a.lens,0,19,a.lencode,0,a.work,zt),a.lenbits=zt.bits,xt){t.msg="invalid code lengths set",a.mode=ot;break}a.have=0,a.mode=J;case J:for(;a.have<a.nlen+a.ndist;){for(;St=a.lencode[_&(1<<a.lenbits)-1],gt=St>>>24,mt=St>>>16&255,wt=65535&St,!(gt<=dt);){if(0===l)break t;l--,_+=n[s++]<<dt,dt+=8}if(wt<16)_>>>=gt,dt-=gt,a.lens[a.have++]=wt;else{if(16===wt){for(Bt=gt+2;dt<Bt;){if(0===l)break t;l--,_+=n[s++]<<dt,dt+=8}if(_>>>=gt,dt-=gt,0===a.have){t.msg="invalid bit length repeat",a.mode=ot;break}yt=a.lens[a.have-1],ut=3+(3&_),_>>>=2,dt-=2}else if(17===wt){for(Bt=gt+3;dt<Bt;){if(0===l)break t;l--,_+=n[s++]<<dt,dt+=8}dt-=gt,yt=0,ut=3+(7&(_>>>=gt)),_>>>=3,dt-=3}else{for(Bt=gt+7;dt<Bt;){if(0===l)break t;l--,_+=n[s++]<<dt,dt+=8}dt-=gt,yt=0,ut=11+(127&(_>>>=gt)),_>>>=7,dt-=7}if(a.have+ut>a.nlen+a.ndist){t.msg="invalid bit length repeat",a.mode=ot;break}for(;ut--;)a.lens[a.have++]=yt}}if(a.mode===ot)break;if(0===a.lens[256]){t.msg="invalid code -- missing end-of-block",a.mode=ot;break}if(a.lenbits=9,zt={bits:a.lenbits},xt=m(p,a.lens,0,a.nlen,a.lencode,0,a.work,zt),a.lenbits=zt.bits,xt){t.msg="invalid literal/lengths set",a.mode=ot;break}if(a.distbits=6,a.distcode=a.distdyn,zt={bits:a.distbits},xt=m(v,a.lens,a.nlen,a.ndist,a.distcode,0,a.work,zt),a.distbits=zt.bits,xt){t.msg="invalid distances set",a.mode=ot;break}if(a.mode=Q,e===x)break t;case Q:a.mode=V;case V:if(l>=6&&f>=258){t.next_out=o,t.avail_out=f,t.next_in=s,t.avail_in=l,a.hold=_,a.bits=dt,g(t,_t),o=t.next_out,r=t.output,f=t.avail_out,s=t.next_in,n=t.input,l=t.avail_in,_=a.hold,dt=a.bits,a.mode===M&&(a.back=-1);break}for(a.back=0;St=a.lencode[_&(1<<a.lenbits)-1],gt=St>>>24,mt=St>>>16&255,wt=65535&St,!(gt<=dt);){if(0===l)break t;l--,_+=n[s++]<<dt,dt+=8}if(mt&&0==(240&mt)){for(pt=gt,vt=mt,kt=wt;St=a.lencode[kt+((_&(1<<pt+vt)-1)>>pt)],gt=St>>>24,mt=St>>>16&255,wt=65535&St,!(pt+gt<=dt);){if(0===l)break t;l--,_+=n[s++]<<dt,dt+=8}_>>>=pt,dt-=pt,a.back+=pt}if(_>>>=gt,dt-=gt,a.back+=gt,a.length=wt,0===mt){a.mode=it;break}if(32&mt){a.back=-1,a.mode=M;break}if(64&mt){t.msg="invalid literal/length code",a.mode=ot;break}a.extra=15&mt,a.mode=$;case $:if(a.extra){for(Bt=a.extra;dt<Bt;){if(0===l)break t;l--,_+=n[s++]<<dt,dt+=8}a.length+=_&(1<<a.extra)-1,_>>>=a.extra,dt-=a.extra,a.back+=a.extra}a.was=a.length,a.mode=tt;case tt:for(;St=a.distcode[_&(1<<a.distbits)-1],gt=St>>>24,mt=St>>>16&255,wt=65535&St,!(gt<=dt);){if(0===l)break t;l--,_+=n[s++]<<dt,dt+=8}if(0==(240&mt)){for(pt=gt,vt=mt,kt=wt;St=a.distcode[kt+((_&(1<<pt+vt)-1)>>pt)],gt=St>>>24,mt=St>>>16&255,wt=65535&St,!(pt+gt<=dt);){if(0===l)break t;l--,_+=n[s++]<<dt,dt+=8}_>>>=pt,dt-=pt,a.back+=pt}if(_>>>=gt,dt-=gt,a.back+=gt,64&mt){t.msg="invalid distance code",a.mode=ot;break}a.offset=wt,a.extra=15&mt,a.mode=et;case et:if(a.extra){for(Bt=a.extra;dt<Bt;){if(0===l)break t;l--,_+=n[s++]<<dt,dt+=8}a.offset+=_&(1<<a.extra)-1,_>>>=a.extra,dt-=a.extra,a.back+=a.extra}if(a.offset>a.dmax){t.msg="invalid distance too far back",a.mode=ot;break}a.mode=at;case at:if(0===f)break t;if(ut=_t-f,a.offset>ut){if((ut=a.offset-ut)>a.whave&&a.sane){t.msg="invalid distance too far back",a.mode=ot;break}ut>a.wnext?(ut-=a.wnext,ct=a.wsize-ut):ct=a.wnext-ut,ut>a.length&&(ut=a.length),bt=a.window}else bt=r,ct=o-a.offset,ut=a.length;ut>f&&(ut=f),f-=ut,a.length-=ut;do{r[o++]=bt[ct++]}while(--ut);0===a.length&&(a.mode=V);break;case it:if(0===f)break t;r[o++]=a.length,f--,a.mode=V;break;case nt:if(a.wrap){for(;dt<32;){if(0===l)break t;l--,_|=n[s++]<<dt,dt+=8}if(_t-=f,t.total_out+=_t,a.total+=_t,_t&&(t.adler=a.check=a.flags?b(a.check,r,_t,o-_t):c(a.check,r,_t,o-_t)),_t=f,(a.flags?_:i(_))!==a.check){t.msg="incorrect data check",a.mode=ot;break}_=0,dt=0}a.mode=rt;case rt:if(a.wrap&&a.flags){for(;dt<32;){if(0===l)break t;l--,_+=n[s++]<<dt,dt+=8}if(_!==(4294967295&a.total)){t.msg="incorrect length check",a.mode=ot;break}_=0,dt=0}a.mode=st;case st:xt=B;break t;case ot:xt=A;break t;case lt:return Z;case ht:default:return E}return t.next_out=o,t.avail_out=f,t.next_in=s,t.avail_in=l,a.hold=_,a.bits=dt,(a.wsize||_t!==t.avail_out&&a.mode<ot&&(a.mode<nt||e!==k))&&d(t,t.output,t.next_out,_t-t.avail_out)?(a.mode=lt,Z):(ft-=t.avail_in,_t-=t.avail_out,t.total_in+=ft,t.total_out+=_t,a.total+=_t,a.wrap&&_t&&(t.adler=a.check=a.flags?b(a.check,r,_t,t.next_out-_t):c(a.check,r,_t,t.next_out-_t)),t.data_type=a.bits+(a.last?64:0)+(a.mode===M?128:0)+(a.mode===Q||a.mode===q?256:0),(0===ft&&0===_t||e===k)&&xt===z&&(xt=R),xt)},a.inflateEnd=function(t){if(!t||!t.state)return E;var e=t.state;return e.window&&(e.window=null),t.state=null,z},a.inflateGetHeader=function(t,e){var a;return t&&t.state?0==(2&(a=t.state).wrap)?E:(a.head=e,e.done=!1,z):E},a.inflateSetDictionary=function(t,e){var a,i,n=e.length;return t&&t.state?0!==(a=t.state).wrap&&a.mode!==K?E:a.mode===K&&(i=1,(i=c(i,e,n,0))!==a.check)?A:d(t,e,n,n)?(a.mode=lt,Z):(a.havedict=1,z):E},a.inflateInfo="pako inflate (from Nodeca project)"},{"../utils/common":3,"./adler32":5,"./crc32":7,"./inffast":10,"./inftrees":12}],12:[function(t,e,a){"use strict";var i=t("../utils/common"),n=[3,4,5,6,7,8,9,10,11,13,15,17,19,23,27,31,35,43,51,59,67,83,99,115,131,163,195,227,258,0,0],r=[16,16,16,16,16,16,16,16,17,17,17,17,18,18,18,18,19,19,19,19,20,20,20,20,21,21,21,21,16,72,78],s=[1,2,3,4,5,7,9,13,17,25,33,49,65,97,129,193,257,385,513,769,1025,1537,2049,3073,4097,6145,8193,12289,16385,24577,0,0],o=[16,16,16,16,17,17,18,18,19,19,20,20,21,21,22,22,23,23,24,24,25,25,26,26,27,27,28,28,29,29,64,64];e.exports=function(t,e,a,l,h,d,f,_){var u,c,b,g,m,w,p,v,k,y=_.bits,x=0,z=0,B=0,S=0,E=0,A=0,Z=0,R=0,C=0,N=0,O=null,D=0,I=new i.Buf16(16),U=new i.Buf16(16),T=null,F=0;for(x=0;x<=15;x++)I[x]=0;for(z=0;z<l;z++)I[e[a+z]]++;for(E=y,S=15;S>=1&&0===I[S];S--);if(E>S&&(E=S),0===S)return h[d++]=20971520,h[d++]=20971520,_.bits=1,0;for(B=1;B<S&&0===I[B];B++);for(E<B&&(E=B),R=1,x=1;x<=15;x++)if(R<<=1,(R-=I[x])<0)return-1;if(R>0&&(0===t||1!==S))return-1;for(U[1]=0,x=1;x<15;x++)U[x+1]=U[x]+I[x];for(z=0;z<l;z++)0!==e[a+z]&&(f[U[e[a+z]]++]=z);if(0===t?(O=T=f,w=19):1===t?(O=n,D-=257,T=r,F-=257,w=256):(O=s,T=o,w=-1),N=0,z=0,x=B,m=d,A=E,Z=0,b=-1,C=1<<E,g=C-1,1===t&&C>852||2===t&&C>592)return 1;for(;;){p=x-Z,f[z]<w?(v=0,k=f[z]):f[z]>w?(v=T[F+f[z]],k=O[D+f[z]]):(v=96,k=0),u=1<<x-Z,B=c=1<<A;do{h[m+(N>>Z)+(c-=u)]=p<<24|v<<16|k|0}while(0!==c);for(u=1<<x-1;N&u;)u>>=1;if(0!==u?(N&=u-1,N+=u):N=0,z++,0==--I[x]){if(x===S)break;x=e[a+f[z]]}if(x>E&&(N&g)!==b){for(0===Z&&(Z=E),m+=B,R=1<<(A=x-Z);A+Z<S&&!((R-=I[A+Z])<=0);)A++,R<<=1;if(C+=1<<A,1===t&&C>852||2===t&&C>592)return 1;h[b=N&g]=E<<24|A<<16|m-d|0}}return 0!==N&&(h[m+N]=x-Z<<24|64<<16|0),_.bits=E,0}},{"../utils/common":3}],13:[function(t,e,a){"use strict";e.exports={2:"need dictionary",1:"stream end",0:"","-1":"file error","-2":"stream error","-3":"data error","-4":"insufficient memory","-5":"buffer error","-6":"incompatible version"}},{}],14:[function(t,e,a){"use strict";function i(t){for(var e=t.length;--e>=0;)t[e]=0}function n(t,e,a,i,n){this.static_tree=t,this.extra_bits=e,this.extra_base=a,this.elems=i,this.max_length=n,this.has_stree=t&&t.length}function r(t,e){this.dyn_tree=t,this.max_code=0,this.stat_desc=e}function s(t){return t<256?et[t]:et[256+(t>>>7)]}function o(t,e){t.pending_buf[t.pending++]=255&e,t.pending_buf[t.pending++]=e>>>8&255}function l(t,e,a){t.bi_valid>M-a?(t.bi_buf|=e<<t.bi_valid&65535,o(t,t.bi_buf),t.bi_buf=e>>M-t.bi_valid,t.bi_valid+=a-M):(t.bi_buf|=e<<t.bi_valid&65535,t.bi_valid+=a)}function h(t,e,a){l(t,a[2*e],a[2*e+1])}function d(t,e){var a=0;do{a|=1&t,t>>>=1,a<<=1}while(--e>0);return a>>>1}function f(t){16===t.bi_valid?(o(t,t.bi_buf),t.bi_buf=0,t.bi_valid=0):t.bi_valid>=8&&(t.pending_buf[t.pending++]=255&t.bi_buf,t.bi_buf>>=8,t.bi_valid-=8)}function _(t,e){var a,i,n,r,s,o,l=e.dyn_tree,h=e.max_code,d=e.stat_desc.static_tree,f=e.stat_desc.has_stree,_=e.stat_desc.extra_bits,u=e.stat_desc.extra_base,c=e.stat_desc.max_length,b=0;for(r=0;r<=K;r++)t.bl_count[r]=0;for(l[2*t.heap[t.heap_max]+1]=0,a=t.heap_max+1;a<j;a++)(r=l[2*l[2*(i=t.heap[a])+1]+1]+1)>c&&(r=c,b++),l[2*i+1]=r,i>h||(t.bl_count[r]++,s=0,i>=u&&(s=_[i-u]),o=l[2*i],t.opt_len+=o*(r+s),f&&(t.static_len+=o*(d[2*i+1]+s)));if(0!==b){do{for(r=c-1;0===t.bl_count[r];)r--;t.bl_count[r]--,t.bl_count[r+1]+=2,t.bl_count[c]--,b-=2}while(b>0);for(r=c;0!==r;r--)for(i=t.bl_count[r];0!==i;)(n=t.heap[--a])>h||(l[2*n+1]!==r&&(t.opt_len+=(r-l[2*n+1])*l[2*n],l[2*n+1]=r),i--)}}function u(t,e,a){var i,n,r=new Array(K+1),s=0;for(i=1;i<=K;i++)r[i]=s=s+a[i-1]<<1;for(n=0;n<=e;n++){var o=t[2*n+1];0!==o&&(t[2*n]=d(r[o]++,o))}}function c(){var t,e,a,i,r,s=new Array(K+1);for(a=0,i=0;i<U-1;i++)for(it[i]=a,t=0;t<1<<W[i];t++)at[a++]=i;for(at[a-1]=i,r=0,i=0;i<16;i++)for(nt[i]=r,t=0;t<1<<J[i];t++)et[r++]=i;for(r>>=7;i<L;i++)for(nt[i]=r<<7,t=0;t<1<<J[i]-7;t++)et[256+r++]=i;for(e=0;e<=K;e++)s[e]=0;for(t=0;t<=143;)$[2*t+1]=8,t++,s[8]++;for(;t<=255;)$[2*t+1]=9,t++,s[9]++;for(;t<=279;)$[2*t+1]=7,t++,s[7]++;for(;t<=287;)$[2*t+1]=8,t++,s[8]++;for(u($,F+1,s),t=0;t<L;t++)tt[2*t+1]=5,tt[2*t]=d(t,5);rt=new n($,W,T+1,F,K),st=new n(tt,J,0,L,K),ot=new n(new Array(0),Q,0,H,P)}function b(t){var e;for(e=0;e<F;e++)t.dyn_ltree[2*e]=0;for(e=0;e<L;e++)t.dyn_dtree[2*e]=0;for(e=0;e<H;e++)t.bl_tree[2*e]=0;t.dyn_ltree[2*Y]=1,t.opt_len=t.static_len=0,t.last_lit=t.matches=0}function g(t){t.bi_valid>8?o(t,t.bi_buf):t.bi_valid>0&&(t.pending_buf[t.pending++]=t.bi_buf),t.bi_buf=0,t.bi_valid=0}function m(t,e,a,i){g(t),i&&(o(t,a),o(t,~a)),A.arraySet(t.pending_buf,t.window,e,a,t.pending),t.pending+=a}function w(t,e,a,i){var n=2*e,r=2*a;return t[n]<t[r]||t[n]===t[r]&&i[e]<=i[a]}function p(t,e,a){for(var i=t.heap[a],n=a<<1;n<=t.heap_len&&(n<t.heap_len&&w(e,t.heap[n+1],t.heap[n],t.depth)&&n++,!w(e,i,t.heap[n],t.depth));)t.heap[a]=t.heap[n],a=n,n<<=1;t.heap[a]=i}function v(t,e,a){var i,n,r,o,d=0;if(0!==t.last_lit)do{i=t.pending_buf[t.d_buf+2*d]<<8|t.pending_buf[t.d_buf+2*d+1],n=t.pending_buf[t.l_buf+d],d++,0===i?h(t,n,e):(h(t,(r=at[n])+T+1,e),0!==(o=W[r])&&l(t,n-=it[r],o),h(t,r=s(--i),a),0!==(o=J[r])&&l(t,i-=nt[r],o))}while(d<t.last_lit);h(t,Y,e)}function k(t,e){var a,i,n,r=e.dyn_tree,s=e.stat_desc.static_tree,o=e.stat_desc.has_stree,l=e.stat_desc.elems,h=-1;for(t.heap_len=0,t.heap_max=j,a=0;a<l;a++)0!==r[2*a]?(t.heap[++t.heap_len]=h=a,t.depth[a]=0):r[2*a+1]=0;for(;t.heap_len<2;)r[2*(n=t.heap[++t.heap_len]=h<2?++h:0)]=1,t.depth[n]=0,t.opt_len--,o&&(t.static_len-=s[2*n+1]);for(e.max_code=h,a=t.heap_len>>1;a>=1;a--)p(t,r,a);n=l;do{a=t.heap[1],t.heap[1]=t.heap[t.heap_len--],p(t,r,1),i=t.heap[1],t.heap[--t.heap_max]=a,t.heap[--t.heap_max]=i,r[2*n]=r[2*a]+r[2*i],t.depth[n]=(t.depth[a]>=t.depth[i]?t.depth[a]:t.depth[i])+1,r[2*a+1]=r[2*i+1]=n,t.heap[1]=n++,p(t,r,1)}while(t.heap_len>=2);t.heap[--t.heap_max]=t.heap[1],_(t,e),u(r,h,t.bl_count)}function y(t,e,a){var i,n,r=-1,s=e[1],o=0,l=7,h=4;for(0===s&&(l=138,h=3),e[2*(a+1)+1]=65535,i=0;i<=a;i++)n=s,s=e[2*(i+1)+1],++o<l&&n===s||(o<h?t.bl_tree[2*n]+=o:0!==n?(n!==r&&t.bl_tree[2*n]++,t.bl_tree[2*q]++):o<=10?t.bl_tree[2*G]++:t.bl_tree[2*X]++,o=0,r=n,0===s?(l=138,h=3):n===s?(l=6,h=3):(l=7,h=4))}function x(t,e,a){var i,n,r=-1,s=e[1],o=0,d=7,f=4;for(0===s&&(d=138,f=3),i=0;i<=a;i++)if(n=s,s=e[2*(i+1)+1],!(++o<d&&n===s)){if(o<f)do{h(t,n,t.bl_tree)}while(0!=--o);else 0!==n?(n!==r&&(h(t,n,t.bl_tree),o--),h(t,q,t.bl_tree),l(t,o-3,2)):o<=10?(h(t,G,t.bl_tree),l(t,o-3,3)):(h(t,X,t.bl_tree),l(t,o-11,7));o=0,r=n,0===s?(d=138,f=3):n===s?(d=6,f=3):(d=7,f=4)}}function z(t){var e;for(y(t,t.dyn_ltree,t.l_desc.max_code),y(t,t.dyn_dtree,t.d_desc.max_code),k(t,t.bl_desc),e=H-1;e>=3&&0===t.bl_tree[2*V[e]+1];e--);return t.opt_len+=3*(e+1)+5+5+4,e}function B(t,e,a,i){var n;for(l(t,e-257,5),l(t,a-1,5),l(t,i-4,4),n=0;n<i;n++)l(t,t.bl_tree[2*V[n]+1],3);x(t,t.dyn_ltree,e-1),x(t,t.dyn_dtree,a-1)}function S(t){var e,a=4093624447;for(e=0;e<=31;e++,a>>>=1)if(1&a&&0!==t.dyn_ltree[2*e])return R;if(0!==t.dyn_ltree[18]||0!==t.dyn_ltree[20]||0!==t.dyn_ltree[26])return C;for(e=32;e<T;e++)if(0!==t.dyn_ltree[2*e])return C;return R}function E(t,e,a,i){l(t,(O<<1)+(i?1:0),3),m(t,e,a,!0)}var A=t("../utils/common"),Z=4,R=0,C=1,N=2,O=0,D=1,I=2,U=29,T=256,F=T+1+U,L=30,H=19,j=2*F+1,K=15,M=16,P=7,Y=256,q=16,G=17,X=18,W=[0,0,0,0,0,0,0,0,1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4,5,5,5,5,0],J=[0,0,0,0,1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9,10,10,11,11,12,12,13,13],Q=[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,3,7],V=[16,17,18,0,8,7,9,6,10,5,11,4,12,3,13,2,14,1,15],$=new Array(2*(F+2));i($);var tt=new Array(2*L);i(tt);var et=new Array(512);i(et);var at=new Array(256);i(at);var it=new Array(U);i(it);var nt=new Array(L);i(nt);var rt,st,ot,lt=!1;a._tr_init=function(t){lt||(c(),lt=!0),t.l_desc=new r(t.dyn_ltree,rt),t.d_desc=new r(t.dyn_dtree,st),t.bl_desc=new r(t.bl_tree,ot),t.bi_buf=0,t.bi_valid=0,b(t)},a._tr_stored_block=E,a._tr_flush_block=function(t,e,a,i){var n,r,s=0;t.level>0?(t.strm.data_type===N&&(t.strm.data_type=S(t)),k(t,t.l_desc),k(t,t.d_desc),s=z(t),n=t.opt_len+3+7>>>3,(r=t.static_len+3+7>>>3)<=n&&(n=r)):n=r=a+5,a+4<=n&&-1!==e?E(t,e,a,i):t.strategy===Z||r===n?(l(t,(D<<1)+(i?1:0),3),v(t,$,tt)):(l(t,(I<<1)+(i?1:0),3),B(t,t.l_desc.max_code+1,t.d_desc.max_code+1,s+1),v(t,t.dyn_ltree,t.dyn_dtree)),b(t),i&&g(t)},a._tr_tally=function(t,e,a){return t.pending_buf[t.d_buf+2*t.last_lit]=e>>>8&255,t.pending_buf[t.d_buf+2*t.last_lit+1]=255&e,t.pending_buf[t.l_buf+t.last_lit]=255&a,t.last_lit++,0===e?t.dyn_ltree[2*a]++:(t.matches++,e--,t.dyn_ltree[2*(at[a]+T+1)]++,t.dyn_dtree[2*s(e)]++),t.last_lit===t.lit_bufsize-1},a._tr_align=function(t){l(t,D<<1,3),h(t,Y,$),f(t)}},{"../utils/common":3}],15:[function(t,e,a){"use strict";e.exports=function(){this.input=null,this.next_in=0,this.avail_in=0,this.total_in=0,this.output=null,this.next_out=0,this.avail_out=0,this.total_out=0,this.msg="",this.state=null,this.data_type=2,this.adler=0}},{}],"/":[function(t,e,a){"use strict";var i={};(0,t("./lib/utils/common").assign)(i,t("./lib/deflate"),t("./lib/inflate"),t("./lib/zlib/constants")),e.exports=i},{"./lib/deflate":1,"./lib/inflate":2,"./lib/utils/common":3,"./lib/zlib/constants":6}]},{},[])("/")});

(function(f) {
  if (typeof exports === "object" && typeof module !== "undefined") {
    module.exports = f();
  } else {
    if (typeof define === "function" && define.amd) {
      define([], f);
    } else {
      var g;
      if (typeof window !== "undefined") {
        g = window;
      } else {
        if (typeof global !== "undefined") {
          g = global;
        } else {
          if (typeof self !== "undefined") {
            g = self;
          } else {
            g = this;
          }
        }
      }
      g.adapter = f();
    }
  }
})(function() {
  var define, module, exports;
  return function e(t, n, r) {
    function s(o, u) {
      if (!n[o]) {
        if (!t[o]) {
          var a = typeof require == "function" && require;
          if (!u && a) {
            return a(o, !0);
          }
          if (i) {
            return i(o, !0);
          }
          var f = new Error("Cannot find module '" + o + "'");
          throw f.code = "MODULE_NOT_FOUND", f;
        }
        var l = n[o] = {exports:{}};
        t[o][0].call(l.exports, function(e) {
          var n = t[o][1][e];
          return s(n ? n : e);
        }, l, l.exports, e, t, n, r);
      }
      return n[o].exports;
    }
    var i = typeof require == "function" && require;
    for (var o = 0; o < r.length; o++) {
      s(r[o]);
    }
    return s;
  }({1:[function(require, module, exports) {
    var SDPUtils = {};
    SDPUtils.generateIdentifier = function() {
      return Math.random().toString(36).substr(2, 10);
    };
    SDPUtils.localCName = SDPUtils.generateIdentifier();
    SDPUtils.splitLines = function(blob) {
      return blob.trim().split("\n").map(function(line) {
        return line.trim();
      });
    };
    SDPUtils.splitSections = function(blob) {
      var parts = blob.split("\nm=");
      return parts.map(function(part, index) {
        return (index > 0 ? "m=" + part : part).trim() + "\r\n";
      });
    };
    SDPUtils.getDescription = function(blob) {
      var sections = SDPUtils.splitSections(blob);
      return sections && sections[0];
    };
    SDPUtils.getMediaSections = function(blob) {
      var sections = SDPUtils.splitSections(blob);
      sections.shift();
      return sections;
    };
    SDPUtils.matchPrefix = function(blob, prefix) {
      return SDPUtils.splitLines(blob).filter(function(line) {
        return line.indexOf(prefix) === 0;
      });
    };
    SDPUtils.parseCandidate = function(line) {
      var parts;
      if (line.indexOf("a=candidate:") === 0) {
        parts = line.substring(12).split(" ");
      } else {
        parts = line.substring(10).split(" ");
      }
      var candidate = {foundation:parts[0], component:parseInt(parts[1], 10), protocol:parts[2].toLowerCase(), priority:parseInt(parts[3], 10), ip:parts[4], port:parseInt(parts[5], 10), type:parts[7]};
      for (var i = 8; i < parts.length; i += 2) {
        switch(parts[i]) {
          case "raddr":
            candidate.relatedAddress = parts[i + 1];
            break;
          case "rport":
            candidate.relatedPort = parseInt(parts[i + 1], 10);
            break;
          case "tcptype":
            candidate.tcpType = parts[i + 1];
            break;
          case "ufrag":
            candidate.ufrag = parts[i + 1];
            candidate.usernameFragment = parts[i + 1];
            break;
          default:
            candidate[parts[i]] = parts[i + 1];
            break;
        }
      }
      return candidate;
    };
    SDPUtils.writeCandidate = function(candidate) {
      var sdp = [];
      sdp.push(candidate.foundation);
      sdp.push(candidate.component);
      sdp.push(candidate.protocol.toUpperCase());
      sdp.push(candidate.priority);
      sdp.push(candidate.ip);
      sdp.push(candidate.port);
      var type = candidate.type;
      sdp.push("typ");
      sdp.push(type);
      if (type !== "host" && candidate.relatedAddress && candidate.relatedPort) {
        sdp.push("raddr");
        sdp.push(candidate.relatedAddress);
        sdp.push("rport");
        sdp.push(candidate.relatedPort);
      }
      if (candidate.tcpType && candidate.protocol.toLowerCase() === "tcp") {
        sdp.push("tcptype");
        sdp.push(candidate.tcpType);
      }
      if (candidate.ufrag) {
        sdp.push("ufrag");
        sdp.push(candidate.ufrag);
      }
      return "candidate:" + sdp.join(" ");
    };
    SDPUtils.parseIceOptions = function(line) {
      return line.substr(14).split(" ");
    };
    SDPUtils.parseRtpMap = function(line) {
      var parts = line.substr(9).split(" ");
      var parsed = {payloadType:parseInt(parts.shift(), 10)};
      parts = parts[0].split("/");
      parsed.name = parts[0];
      parsed.clockRate = parseInt(parts[1], 10);
      parsed.numChannels = parts.length === 3 ? parseInt(parts[2], 10) : 1;
      return parsed;
    };
    SDPUtils.writeRtpMap = function(codec) {
      var pt = codec.payloadType;
      if (codec.preferredPayloadType !== undefined) {
        pt = codec.preferredPayloadType;
      }
      return "a=rtpmap:" + pt + " " + codec.name + "/" + codec.clockRate + (codec.numChannels !== 1 ? "/" + codec.numChannels : "") + "\r\n";
    };
    SDPUtils.parseExtmap = function(line) {
      var parts = line.substr(9).split(" ");
      return {id:parseInt(parts[0], 10), direction:parts[0].indexOf("/") > 0 ? parts[0].split("/")[1] : "sendrecv", uri:parts[1]};
    };
    SDPUtils.writeExtmap = function(headerExtension) {
      return "a=extmap:" + (headerExtension.id || headerExtension.preferredId) + (headerExtension.direction && headerExtension.direction !== "sendrecv" ? "/" + headerExtension.direction : "") + " " + headerExtension.uri + "\r\n";
    };
    SDPUtils.parseFmtp = function(line) {
      var parsed = {};
      var kv;
      var parts = line.substr(line.indexOf(" ") + 1).split(";");
      for (var j = 0; j < parts.length; j++) {
        kv = parts[j].trim().split("=");
        parsed[kv[0].trim()] = kv[1];
      }
      return parsed;
    };
    SDPUtils.writeFmtp = function(codec) {
      var line = "";
      var pt = codec.payloadType;
      if (codec.preferredPayloadType !== undefined) {
        pt = codec.preferredPayloadType;
      }
      if (codec.parameters && Object.keys(codec.parameters).length) {
        var params = [];
        Object.keys(codec.parameters).forEach(function(param) {
          params.push(param + "=" + codec.parameters[param]);
        });
        line += "a=fmtp:" + pt + " " + params.join(";") + "\r\n";
      }
      return line;
    };
    SDPUtils.parseRtcpFb = function(line) {
      var parts = line.substr(line.indexOf(" ") + 1).split(" ");
      return {type:parts.shift(), parameter:parts.join(" ")};
    };
    SDPUtils.writeRtcpFb = function(codec) {
      var lines = "";
      var pt = codec.payloadType;
      if (codec.preferredPayloadType !== undefined) {
        pt = codec.preferredPayloadType;
      }
      if (codec.rtcpFeedback && codec.rtcpFeedback.length) {
        codec.rtcpFeedback.forEach(function(fb) {
          lines += "a=rtcp-fb:" + pt + " " + fb.type + (fb.parameter && fb.parameter.length ? " " + fb.parameter : "") + "\r\n";
        });
      }
      return lines;
    };
    SDPUtils.parseSsrcMedia = function(line) {
      var sp = line.indexOf(" ");
      var parts = {ssrc:parseInt(line.substr(7, sp - 7), 10)};
      var colon = line.indexOf(":", sp);
      if (colon > -1) {
        parts.attribute = line.substr(sp + 1, colon - sp - 1);
        parts.value = line.substr(colon + 1);
      } else {
        parts.attribute = line.substr(sp + 1);
      }
      return parts;
    };
    SDPUtils.getMid = function(mediaSection) {
      var mid = SDPUtils.matchPrefix(mediaSection, "a=mid:")[0];
      if (mid) {
        return mid.substr(6);
      }
    };
    SDPUtils.parseFingerprint = function(line) {
      var parts = line.substr(14).split(" ");
      return {algorithm:parts[0].toLowerCase(), value:parts[1]};
    };
    SDPUtils.getDtlsParameters = function(mediaSection, sessionpart) {
      var lines = SDPUtils.matchPrefix(mediaSection + sessionpart, "a=fingerprint:");
      return {role:"auto", fingerprints:lines.map(SDPUtils.parseFingerprint)};
    };
    SDPUtils.writeDtlsParameters = function(params, setupType) {
      var sdp = "a=setup:" + setupType + "\r\n";
      params.fingerprints.forEach(function(fp) {
        sdp += "a=fingerprint:" + fp.algorithm + " " + fp.value + "\r\n";
      });
      return sdp;
    };
    SDPUtils.getIceParameters = function(mediaSection, sessionpart) {
      var lines = SDPUtils.splitLines(mediaSection);
      lines = lines.concat(SDPUtils.splitLines(sessionpart));
      var iceParameters = {usernameFragment:lines.filter(function(line) {
        return line.indexOf("a=ice-ufrag:") === 0;
      })[0].substr(12), password:lines.filter(function(line) {
        return line.indexOf("a=ice-pwd:") === 0;
      })[0].substr(10)};
      return iceParameters;
    };
    SDPUtils.writeIceParameters = function(params) {
      return "a=ice-ufrag:" + params.usernameFragment + "\r\n" + "a=ice-pwd:" + params.password + "\r\n";
    };
    SDPUtils.parseRtpParameters = function(mediaSection) {
      var description = {codecs:[], headerExtensions:[], fecMechanisms:[], rtcp:[]};
      var lines = SDPUtils.splitLines(mediaSection);
      var mline = lines[0].split(" ");
      for (var i = 3; i < mline.length; i++) {
        var pt = mline[i];
        var rtpmapline = SDPUtils.matchPrefix(mediaSection, "a=rtpmap:" + pt + " ")[0];
        if (rtpmapline) {
          var codec = SDPUtils.parseRtpMap(rtpmapline);
          var fmtps = SDPUtils.matchPrefix(mediaSection, "a=fmtp:" + pt + " ");
          codec.parameters = fmtps.length ? SDPUtils.parseFmtp(fmtps[0]) : {};
          codec.rtcpFeedback = SDPUtils.matchPrefix(mediaSection, "a=rtcp-fb:" + pt + " ").map(SDPUtils.parseRtcpFb);
          description.codecs.push(codec);
          switch(codec.name.toUpperCase()) {
            case "RED":
            case "ULPFEC":
              description.fecMechanisms.push(codec.name.toUpperCase());
              break;
            default:
              break;
          }
        }
      }
      SDPUtils.matchPrefix(mediaSection, "a=extmap:").forEach(function(line) {
        description.headerExtensions.push(SDPUtils.parseExtmap(line));
      });
      return description;
    };
    SDPUtils.writeRtpDescription = function(kind, caps) {
      var sdp = "";
      sdp += "m=" + kind + " ";
      sdp += caps.codecs.length > 0 ? "9" : "0";
      sdp += " UDP/TLS/RTP/SAVPF ";
      sdp += caps.codecs.map(function(codec) {
        if (codec.preferredPayloadType !== undefined) {
          return codec.preferredPayloadType;
        }
        return codec.payloadType;
      }).join(" ") + "\r\n";
      sdp += "c=IN IP4 0.0.0.0\r\n";
      sdp += "a=rtcp:9 IN IP4 0.0.0.0\r\n";
      caps.codecs.forEach(function(codec) {
        sdp += SDPUtils.writeRtpMap(codec);
        sdp += SDPUtils.writeFmtp(codec);
        sdp += SDPUtils.writeRtcpFb(codec);
      });
      var maxptime = 0;
      caps.codecs.forEach(function(codec) {
        if (codec.maxptime > maxptime) {
          maxptime = codec.maxptime;
        }
      });
      if (maxptime > 0) {
        sdp += "a=maxptime:" + maxptime + "\r\n";
      }
      sdp += "a=rtcp-mux\r\n";
      caps.headerExtensions.forEach(function(extension) {
        sdp += SDPUtils.writeExtmap(extension);
      });
      return sdp;
    };
    SDPUtils.parseRtpEncodingParameters = function(mediaSection) {
      var encodingParameters = [];
      var description = SDPUtils.parseRtpParameters(mediaSection);
      var hasRed = description.fecMechanisms.indexOf("RED") !== -1;
      var hasUlpfec = description.fecMechanisms.indexOf("ULPFEC") !== -1;
      var ssrcs = SDPUtils.matchPrefix(mediaSection, "a=ssrc:").map(function(line) {
        return SDPUtils.parseSsrcMedia(line);
      }).filter(function(parts) {
        return parts.attribute === "cname";
      });
      var primarySsrc = ssrcs.length > 0 && ssrcs[0].ssrc;
      var secondarySsrc;
      var flows = SDPUtils.matchPrefix(mediaSection, "a=ssrc-group:FID").map(function(line) {
        var parts = line.split(" ");
        parts.shift();
        return parts.map(function(part) {
          return parseInt(part, 10);
        });
      });
      if (flows.length > 0 && flows[0].length > 1 && flows[0][0] === primarySsrc) {
        secondarySsrc = flows[0][1];
      }
      description.codecs.forEach(function(codec) {
        if (codec.name.toUpperCase() === "RTX" && codec.parameters.apt) {
          var encParam = {ssrc:primarySsrc, codecPayloadType:parseInt(codec.parameters.apt, 10), rtx:{ssrc:secondarySsrc}};
          encodingParameters.push(encParam);
          if (hasRed) {
            encParam = JSON.parse(JSON.stringify(encParam));
            encParam.fec = {ssrc:secondarySsrc, mechanism:hasUlpfec ? "red+ulpfec" : "red"};
            encodingParameters.push(encParam);
          }
        }
      });
      if (encodingParameters.length === 0 && primarySsrc) {
        encodingParameters.push({ssrc:primarySsrc});
      }
      var bandwidth = SDPUtils.matchPrefix(mediaSection, "b=");
      if (bandwidth.length) {
        if (bandwidth[0].indexOf("b=TIAS:") === 0) {
          bandwidth = parseInt(bandwidth[0].substr(7), 10);
        } else {
          if (bandwidth[0].indexOf("b=AS:") === 0) {
            bandwidth = parseInt(bandwidth[0].substr(5), 10) * 1000 * 0.95 - 50 * 40 * 8;
          } else {
            bandwidth = undefined;
          }
        }
        encodingParameters.forEach(function(params) {
          params.maxBitrate = bandwidth;
        });
      }
      return encodingParameters;
    };
    SDPUtils.parseRtcpParameters = function(mediaSection) {
      var rtcpParameters = {};
      var cname;
      var remoteSsrc = SDPUtils.matchPrefix(mediaSection, "a=ssrc:").map(function(line) {
        return SDPUtils.parseSsrcMedia(line);
      }).filter(function(obj) {
        return obj.attribute === "cname";
      })[0];
      if (remoteSsrc) {
        rtcpParameters.cname = remoteSsrc.value;
        rtcpParameters.ssrc = remoteSsrc.ssrc;
      }
      var rsize = SDPUtils.matchPrefix(mediaSection, "a=rtcp-rsize");
      rtcpParameters.reducedSize = rsize.length > 0;
      rtcpParameters.compound = rsize.length === 0;
      var mux = SDPUtils.matchPrefix(mediaSection, "a=rtcp-mux");
      rtcpParameters.mux = mux.length > 0;
      return rtcpParameters;
    };
    SDPUtils.parseMsid = function(mediaSection) {
      var parts;
      var spec = SDPUtils.matchPrefix(mediaSection, "a=msid:");
      if (spec.length === 1) {
        parts = spec[0].substr(7).split(" ");
        return {stream:parts[0], track:parts[1]};
      }
      var planB = SDPUtils.matchPrefix(mediaSection, "a=ssrc:").map(function(line) {
        return SDPUtils.parseSsrcMedia(line);
      }).filter(function(parts) {
        return parts.attribute === "msid";
      });
      if (planB.length > 0) {
        parts = planB[0].value.split(" ");
        return {stream:parts[0], track:parts[1]};
      }
    };
    SDPUtils.generateSessionId = function() {
      return Math.random().toString().substr(2, 21);
    };
    SDPUtils.writeSessionBoilerplate = function(sessId, sessVer) {
      var sessionId;
      var version = sessVer !== undefined ? sessVer : 2;
      if (sessId) {
        sessionId = sessId;
      } else {
        sessionId = SDPUtils.generateSessionId();
      }
      return "v=0\r\n" + "o=thisisadapterortc " + sessionId + " " + version + " IN IP4 127.0.0.1\r\n" + "s=-\r\n" + "t=0 0\r\n";
    };
    SDPUtils.writeMediaSection = function(transceiver, caps, type, stream) {
      var sdp = SDPUtils.writeRtpDescription(transceiver.kind, caps);
      sdp += SDPUtils.writeIceParameters(transceiver.iceGatherer.getLocalParameters());
      sdp += SDPUtils.writeDtlsParameters(transceiver.dtlsTransport.getLocalParameters(), type === "offer" ? "actpass" : "active");
      sdp += "a=mid:" + transceiver.mid + "\r\n";
      if (transceiver.direction) {
        sdp += "a=" + transceiver.direction + "\r\n";
      } else {
        if (transceiver.rtpSender && transceiver.rtpReceiver) {
          sdp += "a=sendrecv\r\n";
        } else {
          if (transceiver.rtpSender) {
            sdp += "a=sendonly\r\n";
          } else {
            if (transceiver.rtpReceiver) {
              sdp += "a=recvonly\r\n";
            } else {
              sdp += "a=inactive\r\n";
            }
          }
        }
      }
      if (transceiver.rtpSender) {
        var msid = "msid:" + stream.id + " " + transceiver.rtpSender.track.id + "\r\n";
        sdp += "a=" + msid;
        sdp += "a=ssrc:" + transceiver.sendEncodingParameters[0].ssrc + " " + msid;
        if (transceiver.sendEncodingParameters[0].rtx) {
          sdp += "a=ssrc:" + transceiver.sendEncodingParameters[0].rtx.ssrc + " " + msid;
          sdp += "a=ssrc-group:FID " + transceiver.sendEncodingParameters[0].ssrc + " " + transceiver.sendEncodingParameters[0].rtx.ssrc + "\r\n";
        }
      }
      sdp += "a=ssrc:" + transceiver.sendEncodingParameters[0].ssrc + " cname:" + SDPUtils.localCName + "\r\n";
      if (transceiver.rtpSender && transceiver.sendEncodingParameters[0].rtx) {
        sdp += "a=ssrc:" + transceiver.sendEncodingParameters[0].rtx.ssrc + " cname:" + SDPUtils.localCName + "\r\n";
      }
      return sdp;
    };
    SDPUtils.getDirection = function(mediaSection, sessionpart) {
      var lines = SDPUtils.splitLines(mediaSection);
      for (var i = 0; i < lines.length; i++) {
        switch(lines[i]) {
          case "a=sendrecv":
          case "a=sendonly":
          case "a=recvonly":
          case "a=inactive":
            return lines[i].substr(2);
          default:
        }
      }
      if (sessionpart) {
        return SDPUtils.getDirection(sessionpart);
      }
      return "sendrecv";
    };
    SDPUtils.getKind = function(mediaSection) {
      var lines = SDPUtils.splitLines(mediaSection);
      var mline = lines[0].split(" ");
      return mline[0].substr(2);
    };
    SDPUtils.isRejected = function(mediaSection) {
      return mediaSection.split(" ", 2)[1] === "0";
    };
    SDPUtils.parseMLine = function(mediaSection) {
      var lines = SDPUtils.splitLines(mediaSection);
      var parts = lines[0].substr(2).split(" ");
      return {kind:parts[0], port:parseInt(parts[1], 10), protocol:parts[2], fmt:parts.slice(3).join(" ")};
    };
    SDPUtils.parseOLine = function(mediaSection) {
      var line = SDPUtils.matchPrefix(mediaSection, "o=")[0];
      var parts = line.substr(2).split(" ");
      return {username:parts[0], sessionId:parts[1], sessionVersion:parseInt(parts[2], 10), netType:parts[3], addressType:parts[4], address:parts[5]};
    };
    if (typeof module === "object") {
      module.exports = SDPUtils;
    }
  }, {}], 2:[function(require, module, exports) {
    var SDPUtils = require("sdp");
    function writeMediaSection(transceiver, caps, type, stream, dtlsRole) {
      var sdp = SDPUtils.writeRtpDescription(transceiver.kind, caps);
      sdp += SDPUtils.writeIceParameters(transceiver.iceGatherer.getLocalParameters());
      sdp += SDPUtils.writeDtlsParameters(transceiver.dtlsTransport.getLocalParameters(), type === "offer" ? "actpass" : dtlsRole || "active");
      sdp += "a=mid:" + transceiver.mid + "\r\n";
      if (transceiver.rtpSender && transceiver.rtpReceiver) {
        sdp += "a=sendrecv\r\n";
      } else {
        if (transceiver.rtpSender) {
          sdp += "a=sendonly\r\n";
        } else {
          if (transceiver.rtpReceiver) {
            sdp += "a=recvonly\r\n";
          } else {
            sdp += "a=inactive\r\n";
          }
        }
      }
      if (transceiver.rtpSender) {
        var trackId = transceiver.rtpSender._initialTrackId || transceiver.rtpSender.track.id;
        transceiver.rtpSender._initialTrackId = trackId;
        var msid = "msid:" + (stream ? stream.id : "-") + " " + trackId + "\r\n";
        sdp += "a=" + msid;
        sdp += "a=ssrc:" + transceiver.sendEncodingParameters[0].ssrc + " " + msid;
        if (transceiver.sendEncodingParameters[0].rtx) {
          sdp += "a=ssrc:" + transceiver.sendEncodingParameters[0].rtx.ssrc + " " + msid;
          sdp += "a=ssrc-group:FID " + transceiver.sendEncodingParameters[0].ssrc + " " + transceiver.sendEncodingParameters[0].rtx.ssrc + "\r\n";
        }
      }
      sdp += "a=ssrc:" + transceiver.sendEncodingParameters[0].ssrc + " cname:" + SDPUtils.localCName + "\r\n";
      if (transceiver.rtpSender && transceiver.sendEncodingParameters[0].rtx) {
        sdp += "a=ssrc:" + transceiver.sendEncodingParameters[0].rtx.ssrc + " cname:" + SDPUtils.localCName + "\r\n";
      }
      return sdp;
    }
    function filterIceServers(iceServers, edgeVersion) {
      var hasTurn = false;
      iceServers = JSON.parse(JSON.stringify(iceServers));
      return iceServers.filter(function(server) {
        if (server && (server.urls || server.url)) {
          var urls = server.urls || server.url;
          if (server.url && !server.urls) {
            console.warn("RTCIceServer.url is deprecated! Use urls instead.");
          }
          var isString = typeof urls === "string";
          if (isString) {
            urls = [urls];
          }
          urls = urls.filter(function(url) {
            var validTurn = url.indexOf("turn:") === 0 && url.indexOf("transport=udp") !== -1 && url.indexOf("turn:[") === -1 && !hasTurn;
            if (validTurn) {
              hasTurn = true;
              return true;
            }
            return url.indexOf("stun:") === 0 && edgeVersion >= 14393 && url.indexOf("?transport=udp") === -1;
          });
          delete server.url;
          server.urls = isString ? urls[0] : urls;
          return !!urls.length;
        }
      });
    }
    function getCommonCapabilities(localCapabilities, remoteCapabilities) {
      var commonCapabilities = {codecs:[], headerExtensions:[], fecMechanisms:[]};
      var findCodecByPayloadType = function(pt, codecs) {
        pt = parseInt(pt, 10);
        for (var i = 0; i < codecs.length; i++) {
          if (codecs[i].payloadType === pt || codecs[i].preferredPayloadType === pt) {
            return codecs[i];
          }
        }
      };
      var rtxCapabilityMatches = function(lRtx, rRtx, lCodecs, rCodecs) {
        var lCodec = findCodecByPayloadType(lRtx.parameters.apt, lCodecs);
        var rCodec = findCodecByPayloadType(rRtx.parameters.apt, rCodecs);
        return lCodec && rCodec && lCodec.name.toLowerCase() === rCodec.name.toLowerCase();
      };
      localCapabilities.codecs.forEach(function(lCodec) {
        for (var i = 0; i < remoteCapabilities.codecs.length; i++) {
          var rCodec = remoteCapabilities.codecs[i];
          if (lCodec.name.toLowerCase() === rCodec.name.toLowerCase() && lCodec.clockRate === rCodec.clockRate) {
            if (lCodec.name.toLowerCase() === "rtx" && lCodec.parameters && rCodec.parameters.apt) {
              if (!rtxCapabilityMatches(lCodec, rCodec, localCapabilities.codecs, remoteCapabilities.codecs)) {
                continue;
              }
            }
            rCodec = JSON.parse(JSON.stringify(rCodec));
            rCodec.numChannels = Math.min(lCodec.numChannels, rCodec.numChannels);
            commonCapabilities.codecs.push(rCodec);
            rCodec.rtcpFeedback = rCodec.rtcpFeedback.filter(function(fb) {
              for (var j = 0; j < lCodec.rtcpFeedback.length; j++) {
                if (lCodec.rtcpFeedback[j].type === fb.type && lCodec.rtcpFeedback[j].parameter === fb.parameter) {
                  return true;
                }
              }
              return false;
            });
            break;
          }
        }
      });
      localCapabilities.headerExtensions.forEach(function(lHeaderExtension) {
        for (var i = 0; i < remoteCapabilities.headerExtensions.length; i++) {
          var rHeaderExtension = remoteCapabilities.headerExtensions[i];
          if (lHeaderExtension.uri === rHeaderExtension.uri) {
            commonCapabilities.headerExtensions.push(rHeaderExtension);
            break;
          }
        }
      });
      return commonCapabilities;
    }
    function isActionAllowedInSignalingState(action, type, signalingState) {
      return {offer:{setLocalDescription:["stable", "have-local-offer"], setRemoteDescription:["stable", "have-remote-offer"]}, answer:{setLocalDescription:["have-remote-offer", "have-local-pranswer"], setRemoteDescription:["have-local-offer", "have-remote-pranswer"]}}[type][action].indexOf(signalingState) !== -1;
    }
    function maybeAddCandidate(iceTransport, candidate) {
      var alreadyAdded = iceTransport.getRemoteCandidates().find(function(remoteCandidate) {
        return candidate.foundation === remoteCandidate.foundation && candidate.ip === remoteCandidate.ip && candidate.port === remoteCandidate.port && candidate.priority === remoteCandidate.priority && candidate.protocol === remoteCandidate.protocol && candidate.type === remoteCandidate.type;
      });
      if (!alreadyAdded) {
        iceTransport.addRemoteCandidate(candidate);
      }
      return !alreadyAdded;
    }
    function makeError(name, description) {
      var e = new Error(description);
      e.name = name;
      e.code = {NotSupportedError:9, InvalidStateError:11, InvalidAccessError:15, TypeError:undefined, OperationError:undefined}[name];
      return e;
    }
    module.exports = function(window, edgeVersion) {
      function addTrackToStreamAndFireEvent(track, stream) {
        stream.addTrack(track);
        stream.dispatchEvent(new window.MediaStreamTrackEvent("addtrack", {track:track}));
      }
      function removeTrackFromStreamAndFireEvent(track, stream) {
        stream.removeTrack(track);
        stream.dispatchEvent(new window.MediaStreamTrackEvent("removetrack", {track:track}));
      }
      function fireAddTrack(pc, track, receiver, streams) {
        var trackEvent = new Event("track");
        trackEvent.track = track;
        trackEvent.receiver = receiver;
        trackEvent.transceiver = {receiver:receiver};
        trackEvent.streams = streams;
        window.setTimeout(function() {
          pc._dispatchEvent("track", trackEvent);
        });
      }
      var RTCPeerConnection = function(config) {
        var pc = this;
        var _eventTarget = document.createDocumentFragment();
        ["addEventListener", "removeEventListener", "dispatchEvent"].forEach(function(method) {
          pc[method] = _eventTarget[method].bind(_eventTarget);
        });
        this.canTrickleIceCandidates = null;
        this.needNegotiation = false;
        this.localStreams = [];
        this.remoteStreams = [];
        this.localDescription = null;
        this.remoteDescription = null;
        this.signalingState = "stable";
        this.iceConnectionState = "new";
        this.connectionState = "new";
        this.iceGatheringState = "new";
        config = JSON.parse(JSON.stringify(config || {}));
        this.usingBundle = config.bundlePolicy === "max-bundle";
        if (config.rtcpMuxPolicy === "negotiate") {
          throw makeError("NotSupportedError", "rtcpMuxPolicy 'negotiate' is not supported");
        } else {
          if (!config.rtcpMuxPolicy) {
            config.rtcpMuxPolicy = "require";
          }
        }
        switch(config.iceTransportPolicy) {
          case "all":
          case "relay":
            break;
          default:
            config.iceTransportPolicy = "all";
            break;
        }
        switch(config.bundlePolicy) {
          case "balanced":
          case "max-compat":
          case "max-bundle":
            break;
          default:
            config.bundlePolicy = "balanced";
            break;
        }
        config.iceServers = filterIceServers(config.iceServers || [], edgeVersion);
        this._iceGatherers = [];
        if (config.iceCandidatePoolSize) {
          for (var i = config.iceCandidatePoolSize; i > 0; i--) {
            this._iceGatherers.push(new window.RTCIceGatherer({iceServers:config.iceServers, gatherPolicy:config.iceTransportPolicy}));
          }
        } else {
          config.iceCandidatePoolSize = 0;
        }
        this._config = config;
        this.transceivers = [];
        this._sdpSessionId = SDPUtils.generateSessionId();
        this._sdpSessionVersion = 0;
        this._dtlsRole = undefined;
        this._isClosed = false;
      };
      RTCPeerConnection.prototype.onicecandidate = null;
      RTCPeerConnection.prototype.onaddstream = null;
      RTCPeerConnection.prototype.ontrack = null;
      RTCPeerConnection.prototype.onremovestream = null;
      RTCPeerConnection.prototype.onsignalingstatechange = null;
      RTCPeerConnection.prototype.oniceconnectionstatechange = null;
      RTCPeerConnection.prototype.onconnectionstatechange = null;
      RTCPeerConnection.prototype.onicegatheringstatechange = null;
      RTCPeerConnection.prototype.onnegotiationneeded = null;
      RTCPeerConnection.prototype.ondatachannel = null;
      RTCPeerConnection.prototype._dispatchEvent = function(name, event) {
        if (this._isClosed) {
          return;
        }
        this.dispatchEvent(event);
        if (typeof this["on" + name] === "function") {
          this["on" + name](event);
        }
      };
      RTCPeerConnection.prototype._emitGatheringStateChange = function() {
        var event = new Event("icegatheringstatechange");
        this._dispatchEvent("icegatheringstatechange", event);
      };
      RTCPeerConnection.prototype.getConfiguration = function() {
        return this._config;
      };
      RTCPeerConnection.prototype.getLocalStreams = function() {
        return this.localStreams;
      };
      RTCPeerConnection.prototype.getRemoteStreams = function() {
        return this.remoteStreams;
      };
      RTCPeerConnection.prototype._createTransceiver = function(kind, doNotAdd) {
        var hasBundleTransport = this.transceivers.length > 0;
        var transceiver = {track:null, iceGatherer:null, iceTransport:null, dtlsTransport:null, localCapabilities:null, remoteCapabilities:null, rtpSender:null, rtpReceiver:null, kind:kind, mid:null, sendEncodingParameters:null, recvEncodingParameters:null, stream:null, associatedRemoteMediaStreams:[], wantReceive:true};
        if (this.usingBundle && hasBundleTransport) {
          transceiver.iceTransport = this.transceivers[0].iceTransport;
          transceiver.dtlsTransport = this.transceivers[0].dtlsTransport;
        } else {
          var transports = this._createIceAndDtlsTransports();
          transceiver.iceTransport = transports.iceTransport;
          transceiver.dtlsTransport = transports.dtlsTransport;
        }
        if (!doNotAdd) {
          this.transceivers.push(transceiver);
        }
        return transceiver;
      };
      RTCPeerConnection.prototype.addTrack = function(track, stream) {
        if (this._isClosed) {
          throw makeError("InvalidStateError", "Attempted to call addTrack on a closed peerconnection.");
        }
        var alreadyExists = this.transceivers.find(function(s) {
          return s.track === track;
        });
        if (alreadyExists) {
          throw makeError("InvalidAccessError", "Track already exists.");
        }
        var transceiver;
        for (var i = 0; i < this.transceivers.length; i++) {
          if (!this.transceivers[i].track && this.transceivers[i].kind === track.kind) {
            transceiver = this.transceivers[i];
          }
        }
        if (!transceiver) {
          transceiver = this._createTransceiver(track.kind);
        }
        this._maybeFireNegotiationNeeded();
        if (this.localStreams.indexOf(stream) === -1) {
          this.localStreams.push(stream);
        }
        transceiver.track = track;
        transceiver.stream = stream;
        transceiver.rtpSender = new window.RTCRtpSender(track, transceiver.dtlsTransport);
        return transceiver.rtpSender;
      };
      RTCPeerConnection.prototype.addStream = function(stream) {
        var pc = this;
        if (edgeVersion >= 15025) {
          stream.getTracks().forEach(function(track) {
            pc.addTrack(track, stream);
          });
        } else {
          var clonedStream = stream.clone();
          stream.getTracks().forEach(function(track, idx) {
            var clonedTrack = clonedStream.getTracks()[idx];
            track.addEventListener("enabled", function(event) {
              clonedTrack.enabled = event.enabled;
            });
          });
          clonedStream.getTracks().forEach(function(track) {
            pc.addTrack(track, clonedStream);
          });
        }
      };
      RTCPeerConnection.prototype.removeTrack = function(sender) {
        if (this._isClosed) {
          throw makeError("InvalidStateError", "Attempted to call removeTrack on a closed peerconnection.");
        }
        if (!(sender instanceof window.RTCRtpSender)) {
          throw new TypeError("Argument 1 of RTCPeerConnection.removeTrack " + "does not implement interface RTCRtpSender.");
        }
        var transceiver = this.transceivers.find(function(t) {
          return t.rtpSender === sender;
        });
        if (!transceiver) {
          throw makeError("InvalidAccessError", "Sender was not created by this connection.");
        }
        var stream = transceiver.stream;
        transceiver.rtpSender.stop();
        transceiver.rtpSender = null;
        transceiver.track = null;
        transceiver.stream = null;
        var localStreams = this.transceivers.map(function(t) {
          return t.stream;
        });
        if (localStreams.indexOf(stream) === -1 && this.localStreams.indexOf(stream) > -1) {
          this.localStreams.splice(this.localStreams.indexOf(stream), 1);
        }
        this._maybeFireNegotiationNeeded();
      };
      RTCPeerConnection.prototype.removeStream = function(stream) {
        var pc = this;
        stream.getTracks().forEach(function(track) {
          var sender = pc.getSenders().find(function(s) {
            return s.track === track;
          });
          if (sender) {
            pc.removeTrack(sender);
          }
        });
      };
      RTCPeerConnection.prototype.getSenders = function() {
        return this.transceivers.filter(function(transceiver) {
          return !!transceiver.rtpSender;
        }).map(function(transceiver) {
          return transceiver.rtpSender;
        });
      };
      RTCPeerConnection.prototype.getReceivers = function() {
        return this.transceivers.filter(function(transceiver) {
          return !!transceiver.rtpReceiver;
        }).map(function(transceiver) {
          return transceiver.rtpReceiver;
        });
      };
      RTCPeerConnection.prototype._createIceGatherer = function(sdpMLineIndex, usingBundle) {
        var pc = this;
        if (usingBundle && sdpMLineIndex > 0) {
          return this.transceivers[0].iceGatherer;
        } else {
          if (this._iceGatherers.length) {
            return this._iceGatherers.shift();
          }
        }
        var iceGatherer = new window.RTCIceGatherer({iceServers:this._config.iceServers, gatherPolicy:this._config.iceTransportPolicy});
        Object.defineProperty(iceGatherer, "state", {value:"new", writable:true});
        this.transceivers[sdpMLineIndex].bufferedCandidateEvents = [];
        this.transceivers[sdpMLineIndex].bufferCandidates = function(event) {
          var end = !event.candidate || Object.keys(event.candidate).length === 0;
          iceGatherer.state = end ? "completed" : "gathering";
          if (pc.transceivers[sdpMLineIndex].bufferedCandidateEvents !== null) {
            pc.transceivers[sdpMLineIndex].bufferedCandidateEvents.push(event);
          }
        };
        iceGatherer.addEventListener("localcandidate", this.transceivers[sdpMLineIndex].bufferCandidates);
        return iceGatherer;
      };
      RTCPeerConnection.prototype._gather = function(mid, sdpMLineIndex) {
        var pc = this;
        var iceGatherer = this.transceivers[sdpMLineIndex].iceGatherer;
        if (iceGatherer.onlocalcandidate) {
          return;
        }
        var bufferedCandidateEvents = this.transceivers[sdpMLineIndex].bufferedCandidateEvents;
        this.transceivers[sdpMLineIndex].bufferedCandidateEvents = null;
        iceGatherer.removeEventListener("localcandidate", this.transceivers[sdpMLineIndex].bufferCandidates);
        iceGatherer.onlocalcandidate = function(evt) {
          if (pc.usingBundle && sdpMLineIndex > 0) {
            return;
          }
          var event = new Event("icecandidate");
          event.candidate = {sdpMid:mid, sdpMLineIndex:sdpMLineIndex};
          var cand = evt.candidate;
          var end = !cand || Object.keys(cand).length === 0;
          if (end) {
            if (iceGatherer.state === "new" || iceGatherer.state === "gathering") {
              iceGatherer.state = "completed";
            }
          } else {
            if (iceGatherer.state === "new") {
              iceGatherer.state = "gathering";
            }
            cand.component = 1;
            var serializedCandidate = SDPUtils.writeCandidate(cand);
            event.candidate = Object.assign(event.candidate, SDPUtils.parseCandidate(serializedCandidate));
            event.candidate.candidate = serializedCandidate;
          }
          var sections = SDPUtils.getMediaSections(pc.localDescription.sdp);
          if (!end) {
            sections[event.candidate.sdpMLineIndex] += "a=" + event.candidate.candidate + "\r\n";
          } else {
            sections[event.candidate.sdpMLineIndex] += "a=end-of-candidates\r\n";
          }
          pc.localDescription.sdp = SDPUtils.getDescription(pc.localDescription.sdp) + sections.join("");
          var complete = pc.transceivers.every(function(transceiver) {
            return transceiver.iceGatherer && transceiver.iceGatherer.state === "completed";
          });
          if (pc.iceGatheringState !== "gathering") {
            pc.iceGatheringState = "gathering";
            pc._emitGatheringStateChange();
          }
          if (!end) {
            pc._dispatchEvent("icecandidate", event);
          }
          if (complete) {
            pc._dispatchEvent("icecandidate", new Event("icecandidate"));
            pc.iceGatheringState = "complete";
            pc._emitGatheringStateChange();
          }
        };
        window.setTimeout(function() {
          bufferedCandidateEvents.forEach(function(e) {
            iceGatherer.onlocalcandidate(e);
          });
        }, 0);
      };
      RTCPeerConnection.prototype._createIceAndDtlsTransports = function() {
        var pc = this;
        var iceTransport = new window.RTCIceTransport(null);
        iceTransport.onicestatechange = function() {
          pc._updateIceConnectionState();
          pc._updateConnectionState();
        };
        var dtlsTransport = new window.RTCDtlsTransport(iceTransport);
        dtlsTransport.ondtlsstatechange = function() {
          pc._updateConnectionState();
        };
        dtlsTransport.onerror = function() {
          Object.defineProperty(dtlsTransport, "state", {value:"failed", writable:true});
          pc._updateConnectionState();
        };
        return {iceTransport:iceTransport, dtlsTransport:dtlsTransport};
      };
      RTCPeerConnection.prototype._disposeIceAndDtlsTransports = function(sdpMLineIndex) {
        var iceGatherer = this.transceivers[sdpMLineIndex].iceGatherer;
        if (iceGatherer) {
          delete iceGatherer.onlocalcandidate;
          delete this.transceivers[sdpMLineIndex].iceGatherer;
        }
        var iceTransport = this.transceivers[sdpMLineIndex].iceTransport;
        if (iceTransport) {
          delete iceTransport.onicestatechange;
          delete this.transceivers[sdpMLineIndex].iceTransport;
        }
        var dtlsTransport = this.transceivers[sdpMLineIndex].dtlsTransport;
        if (dtlsTransport) {
          delete dtlsTransport.ondtlsstatechange;
          delete dtlsTransport.onerror;
          delete this.transceivers[sdpMLineIndex].dtlsTransport;
        }
      };
      RTCPeerConnection.prototype._transceive = function(transceiver, send, recv) {
        var params = getCommonCapabilities(transceiver.localCapabilities, transceiver.remoteCapabilities);
        if (send && transceiver.rtpSender) {
          params.encodings = transceiver.sendEncodingParameters;
          params.rtcp = {cname:SDPUtils.localCName, compound:transceiver.rtcpParameters.compound};
          if (transceiver.recvEncodingParameters.length) {
            params.rtcp.ssrc = transceiver.recvEncodingParameters[0].ssrc;
          }
          transceiver.rtpSender.send(params);
        }
        if (recv && transceiver.rtpReceiver && params.codecs.length > 0) {
          if (transceiver.kind === "video" && transceiver.recvEncodingParameters && edgeVersion < 15019) {
            transceiver.recvEncodingParameters.forEach(function(p) {
              delete p.rtx;
            });
          }
          if (transceiver.recvEncodingParameters.length) {
            params.encodings = transceiver.recvEncodingParameters;
          } else {
            params.encodings = [{}];
          }
          params.rtcp = {compound:transceiver.rtcpParameters.compound};
          if (transceiver.rtcpParameters.cname) {
            params.rtcp.cname = transceiver.rtcpParameters.cname;
          }
          if (transceiver.sendEncodingParameters.length) {
            params.rtcp.ssrc = transceiver.sendEncodingParameters[0].ssrc;
          }
          transceiver.rtpReceiver.receive(params);
        }
      };
      RTCPeerConnection.prototype.setLocalDescription = function(description) {
        var pc = this;
        if (["offer", "answer"].indexOf(description.type) === -1) {
          return Promise.reject(makeError("TypeError", 'Unsupported type "' + description.type + '"'));
        }
        if (!isActionAllowedInSignalingState("setLocalDescription", description.type, pc.signalingState) || pc._isClosed) {
          return Promise.reject(makeError("InvalidStateError", "Can not set local " + description.type + " in state " + pc.signalingState));
        }
        var sections;
        var sessionpart;
        if (description.type === "offer") {
          sections = SDPUtils.splitSections(description.sdp);
          sessionpart = sections.shift();
          sections.forEach(function(mediaSection, sdpMLineIndex) {
            var caps = SDPUtils.parseRtpParameters(mediaSection);
            pc.transceivers[sdpMLineIndex].localCapabilities = caps;
          });
          pc.transceivers.forEach(function(transceiver, sdpMLineIndex) {
            pc._gather(transceiver.mid, sdpMLineIndex);
          });
        } else {
          if (description.type === "answer") {
            sections = SDPUtils.splitSections(pc.remoteDescription.sdp);
            sessionpart = sections.shift();
            var isIceLite = SDPUtils.matchPrefix(sessionpart, "a=ice-lite").length > 0;
            sections.forEach(function(mediaSection, sdpMLineIndex) {
              var transceiver = pc.transceivers[sdpMLineIndex];
              var iceGatherer = transceiver.iceGatherer;
              var iceTransport = transceiver.iceTransport;
              var dtlsTransport = transceiver.dtlsTransport;
              var localCapabilities = transceiver.localCapabilities;
              var remoteCapabilities = transceiver.remoteCapabilities;
              var rejected = SDPUtils.isRejected(mediaSection) && SDPUtils.matchPrefix(mediaSection, "a=bundle-only").length === 0;
              if (!rejected && !transceiver.rejected) {
                var remoteIceParameters = SDPUtils.getIceParameters(mediaSection, sessionpart);
                var remoteDtlsParameters = SDPUtils.getDtlsParameters(mediaSection, sessionpart);
                if (isIceLite) {
                  remoteDtlsParameters.role = "server";
                }
                if (!pc.usingBundle || sdpMLineIndex === 0) {
                  pc._gather(transceiver.mid, sdpMLineIndex);
                  if (iceTransport.state === "new") {
                    iceTransport.start(iceGatherer, remoteIceParameters, isIceLite ? "controlling" : "controlled");
                  }
                  if (dtlsTransport.state === "new") {
                    dtlsTransport.start(remoteDtlsParameters);
                  }
                }
                var params = getCommonCapabilities(localCapabilities, remoteCapabilities);
                pc._transceive(transceiver, params.codecs.length > 0, false);
              }
            });
          }
        }
        pc.localDescription = {type:description.type, sdp:description.sdp};
        if (description.type === "offer") {
          pc._updateSignalingState("have-local-offer");
        } else {
          pc._updateSignalingState("stable");
        }
        return Promise.resolve();
      };
      RTCPeerConnection.prototype.setRemoteDescription = function(description) {
        var pc = this;
        if (["offer", "answer"].indexOf(description.type) === -1) {
          return Promise.reject(makeError("TypeError", 'Unsupported type "' + description.type + '"'));
        }
        if (!isActionAllowedInSignalingState("setRemoteDescription", description.type, pc.signalingState) || pc._isClosed) {
          return Promise.reject(makeError("InvalidStateError", "Can not set remote " + description.type + " in state " + pc.signalingState));
        }
        var streams = {};
        pc.remoteStreams.forEach(function(stream) {
          streams[stream.id] = stream;
        });
        var receiverList = [];
        var sections = SDPUtils.splitSections(description.sdp);
        var sessionpart = sections.shift();
        var isIceLite = SDPUtils.matchPrefix(sessionpart, "a=ice-lite").length > 0;
        var usingBundle = SDPUtils.matchPrefix(sessionpart, "a=group:BUNDLE ").length > 0;
        pc.usingBundle = usingBundle;
        var iceOptions = SDPUtils.matchPrefix(sessionpart, "a=ice-options:")[0];
        if (iceOptions) {
          pc.canTrickleIceCandidates = iceOptions.substr(14).split(" ").indexOf("trickle") >= 0;
        } else {
          pc.canTrickleIceCandidates = false;
        }
        sections.forEach(function(mediaSection, sdpMLineIndex) {
          var lines = SDPUtils.splitLines(mediaSection);
          var kind = SDPUtils.getKind(mediaSection);
          var rejected = SDPUtils.isRejected(mediaSection) && SDPUtils.matchPrefix(mediaSection, "a=bundle-only").length === 0;
          var protocol = lines[0].substr(2).split(" ")[2];
          var direction = SDPUtils.getDirection(mediaSection, sessionpart);
          var remoteMsid = SDPUtils.parseMsid(mediaSection);
          var mid = SDPUtils.getMid(mediaSection) || SDPUtils.generateIdentifier();
          if (kind === "application" && protocol === "DTLS/SCTP" || rejected) {
            pc.transceivers[sdpMLineIndex] = {mid:mid, kind:kind, rejected:true};
            return;
          }
          if (!rejected && pc.transceivers[sdpMLineIndex] && pc.transceivers[sdpMLineIndex].rejected) {
            pc.transceivers[sdpMLineIndex] = pc._createTransceiver(kind, true);
          }
          var transceiver;
          var iceGatherer;
          var iceTransport;
          var dtlsTransport;
          var rtpReceiver;
          var sendEncodingParameters;
          var recvEncodingParameters;
          var localCapabilities;
          var track;
          var remoteCapabilities = SDPUtils.parseRtpParameters(mediaSection);
          var remoteIceParameters;
          var remoteDtlsParameters;
          if (!rejected) {
            remoteIceParameters = SDPUtils.getIceParameters(mediaSection, sessionpart);
            remoteDtlsParameters = SDPUtils.getDtlsParameters(mediaSection, sessionpart);
            remoteDtlsParameters.role = "client";
          }
          recvEncodingParameters = SDPUtils.parseRtpEncodingParameters(mediaSection);
          var rtcpParameters = SDPUtils.parseRtcpParameters(mediaSection);
          var isComplete = SDPUtils.matchPrefix(mediaSection, "a=end-of-candidates", sessionpart).length > 0;
          var cands = SDPUtils.matchPrefix(mediaSection, "a=candidate:").map(function(cand) {
            return SDPUtils.parseCandidate(cand);
          }).filter(function(cand) {
            return cand.component === 1;
          });
          if ((description.type === "offer" || description.type === "answer") && !rejected && usingBundle && sdpMLineIndex > 0 && pc.transceivers[sdpMLineIndex]) {
            pc._disposeIceAndDtlsTransports(sdpMLineIndex);
            pc.transceivers[sdpMLineIndex].iceGatherer = pc.transceivers[0].iceGatherer;
            pc.transceivers[sdpMLineIndex].iceTransport = pc.transceivers[0].iceTransport;
            pc.transceivers[sdpMLineIndex].dtlsTransport = pc.transceivers[0].dtlsTransport;
            if (pc.transceivers[sdpMLineIndex].rtpSender) {
              pc.transceivers[sdpMLineIndex].rtpSender.setTransport(pc.transceivers[0].dtlsTransport);
            }
            if (pc.transceivers[sdpMLineIndex].rtpReceiver) {
              pc.transceivers[sdpMLineIndex].rtpReceiver.setTransport(pc.transceivers[0].dtlsTransport);
            }
          }
          if (description.type === "offer" && !rejected) {
            transceiver = pc.transceivers[sdpMLineIndex] || pc._createTransceiver(kind);
            transceiver.mid = mid;
            if (!transceiver.iceGatherer) {
              transceiver.iceGatherer = pc._createIceGatherer(sdpMLineIndex, usingBundle);
            }
            if (cands.length && transceiver.iceTransport.state === "new") {
              if (isComplete && (!usingBundle || sdpMLineIndex === 0)) {
                transceiver.iceTransport.setRemoteCandidates(cands);
              } else {
                cands.forEach(function(candidate) {
                  maybeAddCandidate(transceiver.iceTransport, candidate);
                });
              }
            }
            localCapabilities = window.RTCRtpReceiver.getCapabilities(kind);
            if (edgeVersion < 15019) {
              localCapabilities.codecs = localCapabilities.codecs.filter(function(codec) {
                return codec.name !== "rtx";
              });
            }
            sendEncodingParameters = transceiver.sendEncodingParameters || [{ssrc:(2 * sdpMLineIndex + 2) * 1001}];
            var isNewTrack = false;
            if (direction === "sendrecv" || direction === "sendonly") {
              isNewTrack = !transceiver.rtpReceiver;
              rtpReceiver = transceiver.rtpReceiver || new window.RTCRtpReceiver(transceiver.dtlsTransport, kind);
              if (isNewTrack) {
                var stream;
                track = rtpReceiver.track;
                if (remoteMsid && remoteMsid.stream === "-") {
                } else {
                  if (remoteMsid) {
                    if (!streams[remoteMsid.stream]) {
                      streams[remoteMsid.stream] = new window.MediaStream;
                      Object.defineProperty(streams[remoteMsid.stream], "id", {get:function() {
                        return remoteMsid.stream;
                      }});
                    }
                    Object.defineProperty(track, "id", {get:function() {
                      return remoteMsid.track;
                    }});
                    stream = streams[remoteMsid.stream];
                  } else {
                    if (!streams.default) {
                      streams.default = new window.MediaStream;
                    }
                    stream = streams.default;
                  }
                }
                if (stream) {
                  addTrackToStreamAndFireEvent(track, stream);
                  transceiver.associatedRemoteMediaStreams.push(stream);
                }
                receiverList.push([track, rtpReceiver, stream]);
              }
            } else {
              if (transceiver.rtpReceiver && transceiver.rtpReceiver.track) {
                transceiver.associatedRemoteMediaStreams.forEach(function(s) {
                  var nativeTrack = s.getTracks().find(function(t) {
                    return t.id === transceiver.rtpReceiver.track.id;
                  });
                  if (nativeTrack) {
                    removeTrackFromStreamAndFireEvent(nativeTrack, s);
                  }
                });
                transceiver.associatedRemoteMediaStreams = [];
              }
            }
            transceiver.localCapabilities = localCapabilities;
            transceiver.remoteCapabilities = remoteCapabilities;
            transceiver.rtpReceiver = rtpReceiver;
            transceiver.rtcpParameters = rtcpParameters;
            transceiver.sendEncodingParameters = sendEncodingParameters;
            transceiver.recvEncodingParameters = recvEncodingParameters;
            pc._transceive(pc.transceivers[sdpMLineIndex], false, isNewTrack);
          } else {
            if (description.type === "answer" && !rejected) {
              transceiver = pc.transceivers[sdpMLineIndex];
              iceGatherer = transceiver.iceGatherer;
              iceTransport = transceiver.iceTransport;
              dtlsTransport = transceiver.dtlsTransport;
              rtpReceiver = transceiver.rtpReceiver;
              sendEncodingParameters = transceiver.sendEncodingParameters;
              localCapabilities = transceiver.localCapabilities;
              pc.transceivers[sdpMLineIndex].recvEncodingParameters = recvEncodingParameters;
              pc.transceivers[sdpMLineIndex].remoteCapabilities = remoteCapabilities;
              pc.transceivers[sdpMLineIndex].rtcpParameters = rtcpParameters;
              if (cands.length && iceTransport.state === "new") {
                if ((isIceLite || isComplete) && (!usingBundle || sdpMLineIndex === 0)) {
                  iceTransport.setRemoteCandidates(cands);
                } else {
                  cands.forEach(function(candidate) {
                    maybeAddCandidate(transceiver.iceTransport, candidate);
                  });
                }
              }
              if (!usingBundle || sdpMLineIndex === 0) {
                if (iceTransport.state === "new") {
                  iceTransport.start(iceGatherer, remoteIceParameters, "controlling");
                }
                if (dtlsTransport.state === "new") {
                  dtlsTransport.start(remoteDtlsParameters);
                }
              }
              pc._transceive(transceiver, direction === "sendrecv" || direction === "recvonly", direction === "sendrecv" || direction === "sendonly");
              if (rtpReceiver && (direction === "sendrecv" || direction === "sendonly")) {
                track = rtpReceiver.track;
                if (remoteMsid) {
                  if (!streams[remoteMsid.stream]) {
                    streams[remoteMsid.stream] = new window.MediaStream;
                  }
                  addTrackToStreamAndFireEvent(track, streams[remoteMsid.stream]);
                  receiverList.push([track, rtpReceiver, streams[remoteMsid.stream]]);
                } else {
                  if (!streams.default) {
                    streams.default = new window.MediaStream;
                  }
                  addTrackToStreamAndFireEvent(track, streams.default);
                  receiverList.push([track, rtpReceiver, streams.default]);
                }
              } else {
                delete transceiver.rtpReceiver;
              }
            }
          }
        });
        if (pc._dtlsRole === undefined) {
          pc._dtlsRole = description.type === "offer" ? "active" : "passive";
        }
        pc.remoteDescription = {type:description.type, sdp:description.sdp};
        if (description.type === "offer") {
          pc._updateSignalingState("have-remote-offer");
        } else {
          pc._updateSignalingState("stable");
        }
        Object.keys(streams).forEach(function(sid) {
          var stream = streams[sid];
          if (stream.getTracks().length) {
            if (pc.remoteStreams.indexOf(stream) === -1) {
              pc.remoteStreams.push(stream);
              var event = new Event("addstream");
              event.stream = stream;
              window.setTimeout(function() {
                pc._dispatchEvent("addstream", event);
              });
            }
            receiverList.forEach(function(item) {
              var track = item[0];
              var receiver = item[1];
              if (stream.id !== item[2].id) {
                return;
              }
              fireAddTrack(pc, track, receiver, [stream]);
            });
          }
        });
        receiverList.forEach(function(item) {
          if (item[2]) {
            return;
          }
          fireAddTrack(pc, item[0], item[1], []);
        });
        window.setTimeout(function() {
          if (!(pc && pc.transceivers)) {
            return;
          }
          pc.transceivers.forEach(function(transceiver) {
            if (transceiver.iceTransport && transceiver.iceTransport.state === "new" && transceiver.iceTransport.getRemoteCandidates().length > 0) {
              console.warn("Timeout for addRemoteCandidate. Consider sending " + "an end-of-candidates notification");
              transceiver.iceTransport.addRemoteCandidate({});
            }
          });
        }, 4000);
        return Promise.resolve();
      };
      RTCPeerConnection.prototype.close = function() {
        this.transceivers.forEach(function(transceiver) {
          if (transceiver.iceTransport) {
            transceiver.iceTransport.stop();
          }
          if (transceiver.dtlsTransport) {
            transceiver.dtlsTransport.stop();
          }
          if (transceiver.rtpSender) {
            transceiver.rtpSender.stop();
          }
          if (transceiver.rtpReceiver) {
            transceiver.rtpReceiver.stop();
          }
        });
        this._isClosed = true;
        this._updateSignalingState("closed");
      };
      RTCPeerConnection.prototype._updateSignalingState = function(newState) {
        this.signalingState = newState;
        var event = new Event("signalingstatechange");
        this._dispatchEvent("signalingstatechange", event);
      };
      RTCPeerConnection.prototype._maybeFireNegotiationNeeded = function() {
        var pc = this;
        if (this.signalingState !== "stable" || this.needNegotiation === true) {
          return;
        }
        this.needNegotiation = true;
        window.setTimeout(function() {
          if (pc.needNegotiation) {
            pc.needNegotiation = false;
            var event = new Event("negotiationneeded");
            pc._dispatchEvent("negotiationneeded", event);
          }
        }, 0);
      };
      RTCPeerConnection.prototype._updateIceConnectionState = function() {
        var newState;
        var states = {"new":0, closed:0, checking:0, connected:0, completed:0, disconnected:0, failed:0};
        this.transceivers.forEach(function(transceiver) {
          states[transceiver.iceTransport.state]++;
        });
        newState = "new";
        if (states.failed > 0) {
          newState = "failed";
        } else {
          if (states.checking > 0) {
            newState = "checking";
          } else {
            if (states.disconnected > 0) {
              newState = "disconnected";
            } else {
              if (states.new > 0) {
                newState = "new";
              } else {
                if (states.connected > 0) {
                  newState = "connected";
                } else {
                  if (states.completed > 0) {
                    newState = "completed";
                  }
                }
              }
            }
          }
        }
        if (newState !== this.iceConnectionState) {
          this.iceConnectionState = newState;
          var event = new Event("iceconnectionstatechange");
          this._dispatchEvent("iceconnectionstatechange", event);
        }
      };
      RTCPeerConnection.prototype._updateConnectionState = function() {
        var newState;
        var states = {"new":0, closed:0, connecting:0, connected:0, completed:0, disconnected:0, failed:0};
        this.transceivers.forEach(function(transceiver) {
          states[transceiver.iceTransport.state]++;
          states[transceiver.dtlsTransport.state]++;
        });
        states.connected += states.completed;
        newState = "new";
        if (states.failed > 0) {
          newState = "failed";
        } else {
          if (states.connecting > 0) {
            newState = "connecting";
          } else {
            if (states.disconnected > 0) {
              newState = "disconnected";
            } else {
              if (states.new > 0) {
                newState = "new";
              } else {
                if (states.connected > 0) {
                  newState = "connected";
                }
              }
            }
          }
        }
        if (newState !== this.connectionState) {
          this.connectionState = newState;
          var event = new Event("connectionstatechange");
          this._dispatchEvent("connectionstatechange", event);
        }
      };
      RTCPeerConnection.prototype.createOffer = function() {
        var pc = this;
        if (pc._isClosed) {
          return Promise.reject(makeError("InvalidStateError", "Can not call createOffer after close"));
        }
        var numAudioTracks = pc.transceivers.filter(function(t) {
          return t.kind === "audio";
        }).length;
        var numVideoTracks = pc.transceivers.filter(function(t) {
          return t.kind === "video";
        }).length;
        var offerOptions = arguments[0];
        if (offerOptions) {
          if (offerOptions.mandatory || offerOptions.optional) {
            throw new TypeError("Legacy mandatory/optional constraints not supported.");
          }
          if (offerOptions.offerToReceiveAudio !== undefined) {
            if (offerOptions.offerToReceiveAudio === true) {
              numAudioTracks = 1;
            } else {
              if (offerOptions.offerToReceiveAudio === false) {
                numAudioTracks = 0;
              } else {
                numAudioTracks = offerOptions.offerToReceiveAudio;
              }
            }
          }
          if (offerOptions.offerToReceiveVideo !== undefined) {
            if (offerOptions.offerToReceiveVideo === true) {
              numVideoTracks = 1;
            } else {
              if (offerOptions.offerToReceiveVideo === false) {
                numVideoTracks = 0;
              } else {
                numVideoTracks = offerOptions.offerToReceiveVideo;
              }
            }
          }
        }
        pc.transceivers.forEach(function(transceiver) {
          if (transceiver.kind === "audio") {
            numAudioTracks--;
            if (numAudioTracks < 0) {
              transceiver.wantReceive = false;
            }
          } else {
            if (transceiver.kind === "video") {
              numVideoTracks--;
              if (numVideoTracks < 0) {
                transceiver.wantReceive = false;
              }
            }
          }
        });
        while (numAudioTracks > 0 || numVideoTracks > 0) {
          if (numAudioTracks > 0) {
            pc._createTransceiver("audio");
            numAudioTracks--;
          }
          if (numVideoTracks > 0) {
            pc._createTransceiver("video");
            numVideoTracks--;
          }
        }
        var sdp = SDPUtils.writeSessionBoilerplate(pc._sdpSessionId, pc._sdpSessionVersion++);
        pc.transceivers.forEach(function(transceiver, sdpMLineIndex) {
          var track = transceiver.track;
          var kind = transceiver.kind;
          var mid = transceiver.mid || SDPUtils.generateIdentifier();
          transceiver.mid = mid;
          if (!transceiver.iceGatherer) {
            transceiver.iceGatherer = pc._createIceGatherer(sdpMLineIndex, pc.usingBundle);
          }
          var localCapabilities = window.RTCRtpSender.getCapabilities(kind);
          if (edgeVersion < 15019) {
            localCapabilities.codecs = localCapabilities.codecs.filter(function(codec) {
              return codec.name !== "rtx";
            });
          }
          localCapabilities.codecs.forEach(function(codec) {
            if (codec.name === "H264" && codec.parameters["level-asymmetry-allowed"] === undefined) {
              codec.parameters["level-asymmetry-allowed"] = "1";
            }
            if (transceiver.remoteCapabilities && transceiver.remoteCapabilities.codecs) {
              transceiver.remoteCapabilities.codecs.forEach(function(remoteCodec) {
                if (codec.name.toLowerCase() === remoteCodec.name.toLowerCase() && codec.clockRate === remoteCodec.clockRate) {
                  codec.preferredPayloadType = remoteCodec.payloadType;
                }
              });
            }
          });
          localCapabilities.headerExtensions.forEach(function(hdrExt) {
            var remoteExtensions = transceiver.remoteCapabilities && transceiver.remoteCapabilities.headerExtensions || [];
            remoteExtensions.forEach(function(rHdrExt) {
              if (hdrExt.uri === rHdrExt.uri) {
                hdrExt.id = rHdrExt.id;
              }
            });
          });
          var sendEncodingParameters = transceiver.sendEncodingParameters || [{ssrc:(2 * sdpMLineIndex + 1) * 1001}];
          if (track) {
            if (edgeVersion >= 15019 && kind === "video" && !sendEncodingParameters[0].rtx) {
              sendEncodingParameters[0].rtx = {ssrc:sendEncodingParameters[0].ssrc + 1};
            }
          }
          if (transceiver.wantReceive) {
            transceiver.rtpReceiver = new window.RTCRtpReceiver(transceiver.dtlsTransport, kind);
          }
          transceiver.localCapabilities = localCapabilities;
          transceiver.sendEncodingParameters = sendEncodingParameters;
        });
        if (pc._config.bundlePolicy !== "max-compat") {
          sdp += "a=group:BUNDLE " + pc.transceivers.map(function(t) {
            return t.mid;
          }).join(" ") + "\r\n";
        }
        sdp += "a=ice-options:trickle\r\n";
        pc.transceivers.forEach(function(transceiver, sdpMLineIndex) {
          sdp += writeMediaSection(transceiver, transceiver.localCapabilities, "offer", transceiver.stream, pc._dtlsRole);
          sdp += "a=rtcp-rsize\r\n";
          if (transceiver.iceGatherer && pc.iceGatheringState !== "new" && (sdpMLineIndex === 0 || !pc.usingBundle)) {
            transceiver.iceGatherer.getLocalCandidates().forEach(function(cand) {
              cand.component = 1;
              sdp += "a=" + SDPUtils.writeCandidate(cand) + "\r\n";
            });
            if (transceiver.iceGatherer.state === "completed") {
              sdp += "a=end-of-candidates\r\n";
            }
          }
        });
        var desc = new window.RTCSessionDescription({type:"offer", sdp:sdp});
        return Promise.resolve(desc);
      };
      RTCPeerConnection.prototype.createAnswer = function() {
        var pc = this;
        if (pc._isClosed) {
          return Promise.reject(makeError("InvalidStateError", "Can not call createAnswer after close"));
        }
        if (!(pc.signalingState === "have-remote-offer" || pc.signalingState === "have-local-pranswer")) {
          return Promise.reject(makeError("InvalidStateError", "Can not call createAnswer in signalingState " + pc.signalingState));
        }
        var sdp = SDPUtils.writeSessionBoilerplate(pc._sdpSessionId, pc._sdpSessionVersion++);
        if (pc.usingBundle) {
          sdp += "a=group:BUNDLE " + pc.transceivers.map(function(t) {
            return t.mid;
          }).join(" ") + "\r\n";
        }
        var mediaSectionsInOffer = SDPUtils.getMediaSections(pc.remoteDescription.sdp).length;
        pc.transceivers.forEach(function(transceiver, sdpMLineIndex) {
          if (sdpMLineIndex + 1 > mediaSectionsInOffer) {
            return;
          }
          if (transceiver.rejected) {
            if (transceiver.kind === "application") {
              sdp += "m=application 0 DTLS/SCTP 5000\r\n";
            } else {
              if (transceiver.kind === "audio") {
                sdp += "m=audio 0 UDP/TLS/RTP/SAVPF 0\r\n" + "a=rtpmap:0 PCMU/8000\r\n";
              } else {
                if (transceiver.kind === "video") {
                  sdp += "m=video 0 UDP/TLS/RTP/SAVPF 120\r\n" + "a=rtpmap:120 VP8/90000\r\n";
                }
              }
            }
            sdp += "c=IN IP4 0.0.0.0\r\n" + "a=inactive\r\n" + "a=mid:" + transceiver.mid + "\r\n";
            return;
          }
          if (transceiver.stream) {
            var localTrack;
            if (transceiver.kind === "audio") {
              localTrack = transceiver.stream.getAudioTracks()[0];
            } else {
              if (transceiver.kind === "video") {
                localTrack = transceiver.stream.getVideoTracks()[0];
              }
            }
            if (localTrack) {
              if (edgeVersion >= 15019 && transceiver.kind === "video" && !transceiver.sendEncodingParameters[0].rtx) {
                transceiver.sendEncodingParameters[0].rtx = {ssrc:transceiver.sendEncodingParameters[0].ssrc + 1};
              }
            }
          }
          var commonCapabilities = getCommonCapabilities(transceiver.localCapabilities, transceiver.remoteCapabilities);
          var hasRtx = commonCapabilities.codecs.filter(function(c) {
            return c.name.toLowerCase() === "rtx";
          }).length;
          if (!hasRtx && transceiver.sendEncodingParameters[0].rtx) {
            delete transceiver.sendEncodingParameters[0].rtx;
          }
          sdp += writeMediaSection(transceiver, commonCapabilities, "answer", transceiver.stream, pc._dtlsRole);
          if (transceiver.rtcpParameters && transceiver.rtcpParameters.reducedSize) {
            sdp += "a=rtcp-rsize\r\n";
          }
        });
        var desc = new window.RTCSessionDescription({type:"answer", sdp:sdp});
        return Promise.resolve(desc);
      };
      RTCPeerConnection.prototype.addIceCandidate = function(candidate) {
        var pc = this;
        var sections;
        if (candidate && !(candidate.sdpMLineIndex !== undefined || candidate.sdpMid)) {
          return Promise.reject(new TypeError("sdpMLineIndex or sdpMid required"));
        }
        return new Promise(function(resolve, reject) {
          if (!pc.remoteDescription) {
            return reject(makeError("InvalidStateError", "Can not add ICE candidate without a remote description"));
          } else {
            if (!candidate || candidate.candidate === "") {
              for (var j = 0; j < pc.transceivers.length; j++) {
                if (pc.transceivers[j].rejected) {
                  continue;
                }
                pc.transceivers[j].iceTransport.addRemoteCandidate({});
                sections = SDPUtils.getMediaSections(pc.remoteDescription.sdp);
                sections[j] += "a=end-of-candidates\r\n";
                pc.remoteDescription.sdp = SDPUtils.getDescription(pc.remoteDescription.sdp) + sections.join("");
                if (pc.usingBundle) {
                  break;
                }
              }
            } else {
              var sdpMLineIndex = candidate.sdpMLineIndex;
              if (candidate.sdpMid) {
                for (var i = 0; i < pc.transceivers.length; i++) {
                  if (pc.transceivers[i].mid === candidate.sdpMid) {
                    sdpMLineIndex = i;
                    break;
                  }
                }
              }
              var transceiver = pc.transceivers[sdpMLineIndex];
              if (transceiver) {
                if (transceiver.rejected) {
                  return resolve();
                }
                var cand = Object.keys(candidate.candidate).length > 0 ? SDPUtils.parseCandidate(candidate.candidate) : {};
                if (cand.protocol === "tcp" && (cand.port === 0 || cand.port === 9)) {
                  return resolve();
                }
                if (cand.component && cand.component !== 1) {
                  return resolve();
                }
                if (sdpMLineIndex === 0 || sdpMLineIndex > 0 && transceiver.iceTransport !== pc.transceivers[0].iceTransport) {
                  if (!maybeAddCandidate(transceiver.iceTransport, cand)) {
                    return reject(makeError("OperationError", "Can not add ICE candidate"));
                  }
                }
                var candidateString = candidate.candidate.trim();
                if (candidateString.indexOf("a=") === 0) {
                  candidateString = candidateString.substr(2);
                }
                sections = SDPUtils.getMediaSections(pc.remoteDescription.sdp);
                sections[sdpMLineIndex] += "a=" + (cand.type ? candidateString : "end-of-candidates") + "\r\n";
                pc.remoteDescription.sdp = sections.join("");
              } else {
                return reject(makeError("OperationError", "Can not add ICE candidate"));
              }
            }
          }
          resolve();
        });
      };
      RTCPeerConnection.prototype.getStats = function() {
        var promises = [];
        this.transceivers.forEach(function(transceiver) {
          ["rtpSender", "rtpReceiver", "iceGatherer", "iceTransport", "dtlsTransport"].forEach(function(method) {
            if (transceiver[method]) {
              promises.push(transceiver[method].getStats());
            }
          });
        });
        var fixStatsType = function(stat) {
          return {inboundrtp:"inbound-rtp", outboundrtp:"outbound-rtp", candidatepair:"candidate-pair", localcandidate:"local-candidate", remotecandidate:"remote-candidate"}[stat.type] || stat.type;
        };
        return new Promise(function(resolve) {
          var results = new Map;
          Promise.all(promises).then(function(res) {
            res.forEach(function(result) {
              Object.keys(result).forEach(function(id) {
                result[id].type = fixStatsType(result[id]);
                results.set(id, result[id]);
              });
            });
            resolve(results);
          });
        });
      };
      var methods = ["createOffer", "createAnswer"];
      methods.forEach(function(method) {
        var nativeMethod = RTCPeerConnection.prototype[method];
        RTCPeerConnection.prototype[method] = function() {
          var args = arguments;
          if (typeof args[0] === "function" || typeof args[1] === "function") {
            return nativeMethod.apply(this, [arguments[2]]).then(function(description) {
              if (typeof args[0] === "function") {
                args[0].apply(null, [description]);
              }
            }, function(error) {
              if (typeof args[1] === "function") {
                args[1].apply(null, [error]);
              }
            });
          }
          return nativeMethod.apply(this, arguments);
        };
      });
      methods = ["setLocalDescription", "setRemoteDescription", "addIceCandidate"];
      methods.forEach(function(method) {
        var nativeMethod = RTCPeerConnection.prototype[method];
        RTCPeerConnection.prototype[method] = function() {
          var args = arguments;
          if (typeof args[1] === "function" || typeof args[2] === "function") {
            return nativeMethod.apply(this, arguments).then(function() {
              if (typeof args[1] === "function") {
                args[1].apply(null);
              }
            }, function(error) {
              if (typeof args[2] === "function") {
                args[2].apply(null, [error]);
              }
            });
          }
          return nativeMethod.apply(this, arguments);
        };
      });
      ["getStats"].forEach(function(method) {
        var nativeMethod = RTCPeerConnection.prototype[method];
        RTCPeerConnection.prototype[method] = function() {
          var args = arguments;
          if (typeof args[1] === "function") {
            return nativeMethod.apply(this, arguments).then(function() {
              if (typeof args[1] === "function") {
                args[1].apply(null);
              }
            });
          }
          return nativeMethod.apply(this, arguments);
        };
      });
      return RTCPeerConnection;
    };
  }, {"sdp":1}], 3:[function(require, module, exports) {
    arguments[4][1][0].apply(exports, arguments);
  }, {"dup":1}], 4:[function(require, module, exports) {
    (function(global) {
      var adapterFactory = require("./adapter_factory.js");
      module.exports = adapterFactory({window:global.window});
    }).call(this, typeof global !== "undefined" ? global : typeof self !== "undefined" ? self : typeof window !== "undefined" ? window : {});
  }, {"./adapter_factory.js":5}], 5:[function(require, module, exports) {
    var utils = require("./utils");
    module.exports = function(dependencies, opts) {
      var window = dependencies && dependencies.window;
      var options = {shimChrome:true, shimFirefox:true, shimEdge:true, shimSafari:true};
      for (var key in opts) {
        if (hasOwnProperty.call(opts, key)) {
          options[key] = opts[key];
        }
      }
      var logging = utils.log;
      var browserDetails = utils.detectBrowser(window);
      var chromeShim = require("./chrome/chrome_shim") || null;
      var edgeShim = require("./edge/edge_shim") || null;
      var firefoxShim = require("./firefox/firefox_shim") || null;
      var safariShim = require("./safari/safari_shim") || null;
      var commonShim = require("./common_shim") || null;
      var adapter = {browserDetails:browserDetails, commonShim:commonShim, extractVersion:utils.extractVersion, disableLog:utils.disableLog, disableWarnings:utils.disableWarnings};
      switch(browserDetails.browser) {
        case "chrome":
          if (!chromeShim || !chromeShim.shimPeerConnection || !options.shimChrome) {
            logging("Chrome shim is not included in this adapter release.");
            return adapter;
          }
          logging("adapter.js shimming chrome.");
          adapter.browserShim = chromeShim;
          commonShim.shimCreateObjectURL(window);
          chromeShim.shimGetUserMedia(window);
          chromeShim.shimMediaStream(window);
          chromeShim.shimSourceObject(window);
          chromeShim.shimPeerConnection(window);
          chromeShim.shimOnTrack(window);
          chromeShim.shimAddTrackRemoveTrack(window);
          chromeShim.shimGetSendersWithDtmf(window);
          commonShim.shimRTCIceCandidate(window);
          commonShim.shimMaxMessageSize(window);
          commonShim.shimSendThrowTypeError(window);
          break;
        case "firefox":
          if (!firefoxShim || !firefoxShim.shimPeerConnection || !options.shimFirefox) {
            logging("Firefox shim is not included in this adapter release.");
            return adapter;
          }
          logging("adapter.js shimming firefox.");
          adapter.browserShim = firefoxShim;
          commonShim.shimCreateObjectURL(window);
          firefoxShim.shimGetUserMedia(window);
          firefoxShim.shimSourceObject(window);
          firefoxShim.shimPeerConnection(window);
          firefoxShim.shimOnTrack(window);
          firefoxShim.shimRemoveStream(window);
          commonShim.shimRTCIceCandidate(window);
          commonShim.shimMaxMessageSize(window);
          commonShim.shimSendThrowTypeError(window);
          break;
        case "edge":
          if (!edgeShim || !edgeShim.shimPeerConnection || !options.shimEdge) {
            logging("MS edge shim is not included in this adapter release.");
            return adapter;
          }
          logging("adapter.js shimming edge.");
          adapter.browserShim = edgeShim;
          commonShim.shimCreateObjectURL(window);
          edgeShim.shimGetUserMedia(window);
          edgeShim.shimPeerConnection(window);
          edgeShim.shimReplaceTrack(window);
          commonShim.shimMaxMessageSize(window);
          commonShim.shimSendThrowTypeError(window);
          break;
        case "safari":
          if (!safariShim || !options.shimSafari) {
            logging("Safari shim is not included in this adapter release.");
            return adapter;
          }
          logging("adapter.js shimming safari.");
          adapter.browserShim = safariShim;
          commonShim.shimCreateObjectURL(window);
          safariShim.shimRTCIceServerUrls(window);
          safariShim.shimCallbacksAPI(window);
          safariShim.shimLocalStreamsAPI(window);
          safariShim.shimRemoteStreamsAPI(window);
          safariShim.shimTrackEventTransceiver(window);
          safariShim.shimGetUserMedia(window);
          safariShim.shimCreateOfferLegacy(window);
          commonShim.shimRTCIceCandidate(window);
          commonShim.shimMaxMessageSize(window);
          commonShim.shimSendThrowTypeError(window);
          break;
        default:
          logging("Unsupported browser!");
          break;
      }
      return adapter;
    };
  }, {"./chrome/chrome_shim":6, "./common_shim":8, "./edge/edge_shim":9, "./firefox/firefox_shim":11, "./safari/safari_shim":13, "./utils":14}], 6:[function(require, module, exports) {
    var utils = require("../utils.js");
    var logging = utils.log;
    module.exports = {shimGetUserMedia:require("./getusermedia"), shimMediaStream:function(window) {
      window.MediaStream = window.MediaStream || window.webkitMediaStream;
    }, shimOnTrack:function(window) {
      if (typeof window === "object" && window.RTCPeerConnection && !("ontrack" in window.RTCPeerConnection.prototype)) {
        Object.defineProperty(window.RTCPeerConnection.prototype, "ontrack", {get:function() {
          return this._ontrack;
        }, set:function(f) {
          if (this._ontrack) {
            this.removeEventListener("track", this._ontrack);
          }
          this.addEventListener("track", this._ontrack = f);
        }});
        var origSetRemoteDescription = window.RTCPeerConnection.prototype.setRemoteDescription;
        window.RTCPeerConnection.prototype.setRemoteDescription = function() {
          var pc = this;
          if (!pc._ontrackpoly) {
            pc._ontrackpoly = function(e) {
              e.stream.addEventListener("addtrack", function(te) {
                var receiver;
                if (window.RTCPeerConnection.prototype.getReceivers) {
                  receiver = pc.getReceivers().find(function(r) {
                    return r.track && r.track.id === te.track.id;
                  });
                } else {
                  receiver = {track:te.track};
                }
                var event = new Event("track");
                event.track = te.track;
                event.receiver = receiver;
                event.transceiver = {receiver:receiver};
                event.streams = [e.stream];
                pc.dispatchEvent(event);
              });
              e.stream.getTracks().forEach(function(track) {
                var receiver;
                if (window.RTCPeerConnection.prototype.getReceivers) {
                  receiver = pc.getReceivers().find(function(r) {
                    return r.track && r.track.id === track.id;
                  });
                } else {
                  receiver = {track:track};
                }
                var event = new Event("track");
                event.track = track;
                event.receiver = receiver;
                event.transceiver = {receiver:receiver};
                event.streams = [e.stream];
                pc.dispatchEvent(event);
              });
            };
            pc.addEventListener("addstream", pc._ontrackpoly);
          }
          return origSetRemoteDescription.apply(pc, arguments);
        };
      } else {
        if (!("RTCRtpTransceiver" in window)) {
          utils.wrapPeerConnectionEvent(window, "track", function(e) {
            if (!e.transceiver) {
              e.transceiver = {receiver:e.receiver};
            }
            return e;
          });
        }
      }
    }, shimGetSendersWithDtmf:function(window) {
      if (typeof window === "object" && window.RTCPeerConnection && !("getSenders" in window.RTCPeerConnection.prototype) && "createDTMFSender" in window.RTCPeerConnection.prototype) {
        var shimSenderWithDtmf = function(pc, track) {
          return {track:track, get dtmf() {
            if (this._dtmf === undefined) {
              if (track.kind === "audio") {
                this._dtmf = pc.createDTMFSender(track);
              } else {
                this._dtmf = null;
              }
            }
            return this._dtmf;
          }, _pc:pc};
        };
        if (!window.RTCPeerConnection.prototype.getSenders) {
          window.RTCPeerConnection.prototype.getSenders = function() {
            this._senders = this._senders || [];
            return this._senders.slice();
          };
          var origAddTrack = window.RTCPeerConnection.prototype.addTrack;
          window.RTCPeerConnection.prototype.addTrack = function(track, stream) {
            var pc = this;
            var sender = origAddTrack.apply(pc, arguments);
            if (!sender) {
              sender = shimSenderWithDtmf(pc, track);
              pc._senders.push(sender);
            }
            return sender;
          };
          var origRemoveTrack = window.RTCPeerConnection.prototype.removeTrack;
          window.RTCPeerConnection.prototype.removeTrack = function(sender) {
            var pc = this;
            origRemoveTrack.apply(pc, arguments);
            var idx = pc._senders.indexOf(sender);
            if (idx !== -1) {
              pc._senders.splice(idx, 1);
            }
          };
        }
        var origAddStream = window.RTCPeerConnection.prototype.addStream;
        window.RTCPeerConnection.prototype.addStream = function(stream) {
          var pc = this;
          pc._senders = pc._senders || [];
          origAddStream.apply(pc, [stream]);
          stream.getTracks().forEach(function(track) {
            pc._senders.push(shimSenderWithDtmf(pc, track));
          });
        };
        var origRemoveStream = window.RTCPeerConnection.prototype.removeStream;
        window.RTCPeerConnection.prototype.removeStream = function(stream) {
          var pc = this;
          pc._senders = pc._senders || [];
          origRemoveStream.apply(pc, [stream]);
          stream.getTracks().forEach(function(track) {
            var sender = pc._senders.find(function(s) {
              return s.track === track;
            });
            if (sender) {
              pc._senders.splice(pc._senders.indexOf(sender), 1);
            }
          });
        };
      } else {
        if (typeof window === "object" && window.RTCPeerConnection && "getSenders" in window.RTCPeerConnection.prototype && "createDTMFSender" in window.RTCPeerConnection.prototype && window.RTCRtpSender && !("dtmf" in window.RTCRtpSender.prototype)) {
          var origGetSenders = window.RTCPeerConnection.prototype.getSenders;
          window.RTCPeerConnection.prototype.getSenders = function() {
            var pc = this;
            var senders = origGetSenders.apply(pc, []);
            senders.forEach(function(sender) {
              sender._pc = pc;
            });
            return senders;
          };
          Object.defineProperty(window.RTCRtpSender.prototype, "dtmf", {get:function() {
            if (this._dtmf === undefined) {
              if (this.track.kind === "audio") {
                this._dtmf = this._pc.createDTMFSender(this.track);
              } else {
                this._dtmf = null;
              }
            }
            return this._dtmf;
          }});
        }
      }
    }, shimSourceObject:function(window) {
      var URL = window && window.URL;
      if (typeof window === "object") {
        if (window.HTMLMediaElement && !("srcObject" in window.HTMLMediaElement.prototype)) {
          Object.defineProperty(window.HTMLMediaElement.prototype, "srcObject", {get:function() {
            return this._srcObject;
          }, set:function(stream) {
            var self = this;
            this._srcObject = stream;
            if (this.src) {
              URL.revokeObjectURL(this.src);
            }
            if (!stream) {
              this.src = "";
              return undefined;
            }
            this.src = URL.createObjectURL(stream);
            stream.addEventListener("addtrack", function() {
              if (self.src) {
                URL.revokeObjectURL(self.src);
              }
              self.src = URL.createObjectURL(stream);
            });
            stream.addEventListener("removetrack", function() {
              if (self.src) {
                URL.revokeObjectURL(self.src);
              }
              self.src = URL.createObjectURL(stream);
            });
          }});
        }
      }
    }, shimAddTrackRemoveTrackWithNative:function(window) {
      window.RTCPeerConnection.prototype.getLocalStreams = function() {
        var pc = this;
        this._shimmedLocalStreams = this._shimmedLocalStreams || {};
        return Object.keys(this._shimmedLocalStreams).map(function(streamId) {
          return pc._shimmedLocalStreams[streamId][0];
        });
      };
      var origAddTrack = window.RTCPeerConnection.prototype.addTrack;
      window.RTCPeerConnection.prototype.addTrack = function(track, stream) {
        if (!stream) {
          return origAddTrack.apply(this, arguments);
        }
        this._shimmedLocalStreams = this._shimmedLocalStreams || {};
        var sender = origAddTrack.apply(this, arguments);
        if (!this._shimmedLocalStreams[stream.id]) {
          this._shimmedLocalStreams[stream.id] = [stream, sender];
        } else {
          if (this._shimmedLocalStreams[stream.id].indexOf(sender) === -1) {
            this._shimmedLocalStreams[stream.id].push(sender);
          }
        }
        return sender;
      };
      var origAddStream = window.RTCPeerConnection.prototype.addStream;
      window.RTCPeerConnection.prototype.addStream = function(stream) {
        var pc = this;
        this._shimmedLocalStreams = this._shimmedLocalStreams || {};
        stream.getTracks().forEach(function(track) {
          var alreadyExists = pc.getSenders().find(function(s) {
            return s.track === track;
          });
          if (alreadyExists) {
            throw new DOMException("Track already exists.", "InvalidAccessError");
          }
        });
        var existingSenders = pc.getSenders();
        origAddStream.apply(this, arguments);
        var newSenders = pc.getSenders().filter(function(newSender) {
          return existingSenders.indexOf(newSender) === -1;
        });
        this._shimmedLocalStreams[stream.id] = [stream].concat(newSenders);
      };
      var origRemoveStream = window.RTCPeerConnection.prototype.removeStream;
      window.RTCPeerConnection.prototype.removeStream = function(stream) {
        this._shimmedLocalStreams = this._shimmedLocalStreams || {};
        delete this._shimmedLocalStreams[stream.id];
        return origRemoveStream.apply(this, arguments);
      };
      var origRemoveTrack = window.RTCPeerConnection.prototype.removeTrack;
      window.RTCPeerConnection.prototype.removeTrack = function(sender) {
        var pc = this;
        this._shimmedLocalStreams = this._shimmedLocalStreams || {};
        if (sender) {
          Object.keys(this._shimmedLocalStreams).forEach(function(streamId) {
            var idx = pc._shimmedLocalStreams[streamId].indexOf(sender);
            if (idx !== -1) {
              pc._shimmedLocalStreams[streamId].splice(idx, 1);
            }
            if (pc._shimmedLocalStreams[streamId].length === 1) {
              delete pc._shimmedLocalStreams[streamId];
            }
          });
        }
        return origRemoveTrack.apply(this, arguments);
      };
    }, shimAddTrackRemoveTrack:function(window) {
      var browserDetails = utils.detectBrowser(window);
      if (window.RTCPeerConnection.prototype.addTrack && browserDetails.version >= 65) {
        return this.shimAddTrackRemoveTrackWithNative(window);
      }
      var origGetLocalStreams = window.RTCPeerConnection.prototype.getLocalStreams;
      window.RTCPeerConnection.prototype.getLocalStreams = function() {
        var pc = this;
        var nativeStreams = origGetLocalStreams.apply(this);
        pc._reverseStreams = pc._reverseStreams || {};
        return nativeStreams.map(function(stream) {
          return pc._reverseStreams[stream.id];
        });
      };
      var origAddStream = window.RTCPeerConnection.prototype.addStream;
      window.RTCPeerConnection.prototype.addStream = function(stream) {
        var pc = this;
        pc._streams = pc._streams || {};
        pc._reverseStreams = pc._reverseStreams || {};
        stream.getTracks().forEach(function(track) {
          var alreadyExists = pc.getSenders().find(function(s) {
            return s.track === track;
          });
          if (alreadyExists) {
            throw new DOMException("Track already exists.", "InvalidAccessError");
          }
        });
        if (!pc._reverseStreams[stream.id]) {
          var newStream = new window.MediaStream(stream.getTracks());
          pc._streams[stream.id] = newStream;
          pc._reverseStreams[newStream.id] = stream;
          stream = newStream;
        }
        origAddStream.apply(pc, [stream]);
      };
      var origRemoveStream = window.RTCPeerConnection.prototype.removeStream;
      window.RTCPeerConnection.prototype.removeStream = function(stream) {
        var pc = this;
        pc._streams = pc._streams || {};
        pc._reverseStreams = pc._reverseStreams || {};
        origRemoveStream.apply(pc, [pc._streams[stream.id] || stream]);
        delete pc._reverseStreams[pc._streams[stream.id] ? pc._streams[stream.id].id : stream.id];
        delete pc._streams[stream.id];
      };
      window.RTCPeerConnection.prototype.addTrack = function(track, stream) {
        var pc = this;
        if (pc.signalingState === "closed") {
          throw new DOMException("The RTCPeerConnection's signalingState is 'closed'.", "InvalidStateError");
        }
        var streams = [].slice.call(arguments, 1);
        if (streams.length !== 1 || !streams[0].getTracks().find(function(t) {
          return t === track;
        })) {
          throw new DOMException("The adapter.js addTrack polyfill only supports a single " + " stream which is associated with the specified track.", "NotSupportedError");
        }
        var alreadyExists = pc.getSenders().find(function(s) {
          return s.track === track;
        });
        if (alreadyExists) {
          throw new DOMException("Track already exists.", "InvalidAccessError");
        }
        pc._streams = pc._streams || {};
        pc._reverseStreams = pc._reverseStreams || {};
        var oldStream = pc._streams[stream.id];
        if (oldStream) {
          oldStream.addTrack(track);
          Promise.resolve().then(function() {
            pc.dispatchEvent(new Event("negotiationneeded"));
          });
        } else {
          var newStream = new window.MediaStream([track]);
          pc._streams[stream.id] = newStream;
          pc._reverseStreams[newStream.id] = stream;
          pc.addStream(newStream);
        }
        return pc.getSenders().find(function(s) {
          return s.track === track;
        });
      };
      function replaceInternalStreamId(pc, description) {
        var sdp = description.sdp;
        Object.keys(pc._reverseStreams || []).forEach(function(internalId) {
          var externalStream = pc._reverseStreams[internalId];
          var internalStream = pc._streams[externalStream.id];
          sdp = sdp.replace(new RegExp(internalStream.id, "g"), externalStream.id);
        });
        return new RTCSessionDescription({type:description.type, sdp:sdp});
      }
      function replaceExternalStreamId(pc, description) {
        var sdp = description.sdp;
        Object.keys(pc._reverseStreams || []).forEach(function(internalId) {
          var externalStream = pc._reverseStreams[internalId];
          var internalStream = pc._streams[externalStream.id];
          sdp = sdp.replace(new RegExp(externalStream.id, "g"), internalStream.id);
        });
        return new RTCSessionDescription({type:description.type, sdp:sdp});
      }
      ["createOffer", "createAnswer"].forEach(function(method) {
        var nativeMethod = window.RTCPeerConnection.prototype[method];
        window.RTCPeerConnection.prototype[method] = function() {
          var pc = this;
          var args = arguments;
          var isLegacyCall = arguments.length && typeof arguments[0] === "function";
          if (isLegacyCall) {
            return nativeMethod.apply(pc, [function(description) {
              var desc = replaceInternalStreamId(pc, description);
              args[0].apply(null, [desc]);
            }, function(err) {
              if (args[1]) {
                args[1].apply(null, err);
              }
            }, arguments[2]]);
          }
          return nativeMethod.apply(pc, arguments).then(function(description) {
            return replaceInternalStreamId(pc, description);
          });
        };
      });
      var origSetLocalDescription = window.RTCPeerConnection.prototype.setLocalDescription;
      window.RTCPeerConnection.prototype.setLocalDescription = function() {
        var pc = this;
        if (!arguments.length || !arguments[0].type) {
          return origSetLocalDescription.apply(pc, arguments);
        }
        arguments[0] = replaceExternalStreamId(pc, arguments[0]);
        return origSetLocalDescription.apply(pc, arguments);
      };
      var origLocalDescription = Object.getOwnPropertyDescriptor(window.RTCPeerConnection.prototype, "localDescription");
      Object.defineProperty(window.RTCPeerConnection.prototype, "localDescription", {get:function() {
        var pc = this;
        var description = origLocalDescription.get.apply(this);
        if (description.type === "") {
          return description;
        }
        return replaceInternalStreamId(pc, description);
      }});
      window.RTCPeerConnection.prototype.removeTrack = function(sender) {
        var pc = this;
        if (pc.signalingState === "closed") {
          throw new DOMException("The RTCPeerConnection's signalingState is 'closed'.", "InvalidStateError");
        }
        if (!sender._pc) {
          throw new DOMException("Argument 1 of RTCPeerConnection.removeTrack " + "does not implement interface RTCRtpSender.", "TypeError");
        }
        var isLocal = sender._pc === pc;
        if (!isLocal) {
          throw new DOMException("Sender was not created by this connection.", "InvalidAccessError");
        }
        pc._streams = pc._streams || {};
        var stream;
        Object.keys(pc._streams).forEach(function(streamid) {
          var hasTrack = pc._streams[streamid].getTracks().find(function(track) {
            return sender.track === track;
          });
          if (hasTrack) {
            stream = pc._streams[streamid];
          }
        });
        if (stream) {
          if (stream.getTracks().length === 1) {
            pc.removeStream(pc._reverseStreams[stream.id]);
          } else {
            stream.removeTrack(sender.track);
          }
          pc.dispatchEvent(new Event("negotiationneeded"));
        }
      };
    }, shimPeerConnection:function(window) {
      var browserDetails = utils.detectBrowser(window);
      if (!window.RTCPeerConnection && window.webkitRTCPeerConnection) {
        window.RTCPeerConnection = function(pcConfig, pcConstraints) {
          logging("PeerConnection");
          if (pcConfig && pcConfig.iceTransportPolicy) {
            pcConfig.iceTransports = pcConfig.iceTransportPolicy;
          }
          return new window.webkitRTCPeerConnection(pcConfig, pcConstraints);
        };
        window.RTCPeerConnection.prototype = window.webkitRTCPeerConnection.prototype;
        if (window.webkitRTCPeerConnection.generateCertificate) {
          Object.defineProperty(window.RTCPeerConnection, "generateCertificate", {get:function() {
            return window.webkitRTCPeerConnection.generateCertificate;
          }});
        }
      } else {
        var OrigPeerConnection = window.RTCPeerConnection;
        window.RTCPeerConnection = function(pcConfig, pcConstraints) {
          if (pcConfig && pcConfig.iceServers) {
            var newIceServers = [];
            for (var i = 0; i < pcConfig.iceServers.length; i++) {
              var server = pcConfig.iceServers[i];
              if (!server.hasOwnProperty("urls") && server.hasOwnProperty("url")) {
                utils.deprecated("RTCIceServer.url", "RTCIceServer.urls");
                server = JSON.parse(JSON.stringify(server));
                server.urls = server.url;
                newIceServers.push(server);
              } else {
                newIceServers.push(pcConfig.iceServers[i]);
              }
            }
            pcConfig.iceServers = newIceServers;
          }
          return new OrigPeerConnection(pcConfig, pcConstraints);
        };
        window.RTCPeerConnection.prototype = OrigPeerConnection.prototype;
        Object.defineProperty(window.RTCPeerConnection, "generateCertificate", {get:function() {
          return OrigPeerConnection.generateCertificate;
        }});
      }
      var origGetStats = window.RTCPeerConnection.prototype.getStats;
      window.RTCPeerConnection.prototype.getStats = function(selector, successCallback, errorCallback) {
        var pc = this;
        var args = arguments;
        if (arguments.length > 0 && typeof selector === "function") {
          return origGetStats.apply(this, arguments);
        }
        if (origGetStats.length === 0 && (arguments.length === 0 || typeof arguments[0] !== "function")) {
          return origGetStats.apply(this, []);
        }
        var fixChromeStats_ = function(response) {
          var standardReport = {};
          var reports = response.result();
          reports.forEach(function(report) {
            var standardStats = {id:report.id, timestamp:report.timestamp, type:{localcandidate:"local-candidate", remotecandidate:"remote-candidate"}[report.type] || report.type};
            report.names().forEach(function(name) {
              standardStats[name] = report.stat(name);
            });
            standardReport[standardStats.id] = standardStats;
          });
          return standardReport;
        };
        var makeMapStats = function(stats) {
          return new Map(Object.keys(stats).map(function(key) {
            return [key, stats[key]];
          }));
        };
        if (arguments.length >= 2) {
          var successCallbackWrapper_ = function(response) {
            args[1](makeMapStats(fixChromeStats_(response)));
          };
          return origGetStats.apply(this, [successCallbackWrapper_, arguments[0]]);
        }
        return (new Promise(function(resolve, reject) {
          origGetStats.apply(pc, [function(response) {
            resolve(makeMapStats(fixChromeStats_(response)));
          }, reject]);
        })).then(successCallback, errorCallback);
      };
      if (browserDetails.version < 51) {
        ["setLocalDescription", "setRemoteDescription", "addIceCandidate"].forEach(function(method) {
          var nativeMethod = window.RTCPeerConnection.prototype[method];
          window.RTCPeerConnection.prototype[method] = function() {
            var args = arguments;
            var pc = this;
            var promise = new Promise(function(resolve, reject) {
              nativeMethod.apply(pc, [args[0], resolve, reject]);
            });
            if (args.length < 2) {
              return promise;
            }
            return promise.then(function() {
              args[1].apply(null, []);
            }, function(err) {
              if (args.length >= 3) {
                args[2].apply(null, [err]);
              }
            });
          };
        });
      }
      if (browserDetails.version < 52) {
        ["createOffer", "createAnswer"].forEach(function(method) {
          var nativeMethod = window.RTCPeerConnection.prototype[method];
          window.RTCPeerConnection.prototype[method] = function() {
            var pc = this;
            if (arguments.length < 1 || arguments.length === 1 && typeof arguments[0] === "object") {
              var opts = arguments.length === 1 ? arguments[0] : undefined;
              return new Promise(function(resolve, reject) {
                nativeMethod.apply(pc, [resolve, reject, opts]);
              });
            }
            return nativeMethod.apply(this, arguments);
          };
        });
      }
      ["setLocalDescription", "setRemoteDescription", "addIceCandidate"].forEach(function(method) {
        var nativeMethod = window.RTCPeerConnection.prototype[method];
        window.RTCPeerConnection.prototype[method] = function() {
          arguments[0] = new (method === "addIceCandidate" ? window.RTCIceCandidate : window.RTCSessionDescription)(arguments[0]);
          return nativeMethod.apply(this, arguments);
        };
      });
      var nativeAddIceCandidate = window.RTCPeerConnection.prototype.addIceCandidate;
      window.RTCPeerConnection.prototype.addIceCandidate = function() {
        if (!arguments[0]) {
          if (arguments[1]) {
            arguments[1].apply(null);
          }
          return Promise.resolve();
        }
        return nativeAddIceCandidate.apply(this, arguments);
      };
    }};
  }, {"../utils.js":14, "./getusermedia":7}], 7:[function(require, module, exports) {
    var utils = require("../utils.js");
    var logging = utils.log;
    module.exports = function(window) {
      var browserDetails = utils.detectBrowser(window);
      var navigator = window && window.navigator;
      var constraintsToChrome_ = function(c) {
        if (typeof c !== "object" || c.mandatory || c.optional) {
          return c;
        }
        var cc = {};
        Object.keys(c).forEach(function(key) {
          if (key === "require" || key === "advanced" || key === "mediaSource") {
            return;
          }
          var r = typeof c[key] === "object" ? c[key] : {ideal:c[key]};
          if (r.exact !== undefined && typeof r.exact === "number") {
            r.min = r.max = r.exact;
          }
          var oldname_ = function(prefix, name) {
            if (prefix) {
              return prefix + name.charAt(0).toUpperCase() + name.slice(1);
            }
            return name === "deviceId" ? "sourceId" : name;
          };
          if (r.ideal !== undefined) {
            cc.optional = cc.optional || [];
            var oc = {};
            if (typeof r.ideal === "number") {
              oc[oldname_("min", key)] = r.ideal;
              cc.optional.push(oc);
              oc = {};
              oc[oldname_("max", key)] = r.ideal;
              cc.optional.push(oc);
            } else {
              oc[oldname_("", key)] = r.ideal;
              cc.optional.push(oc);
            }
          }
          if (r.exact !== undefined && typeof r.exact !== "number") {
            cc.mandatory = cc.mandatory || {};
            cc.mandatory[oldname_("", key)] = r.exact;
          } else {
            ["min", "max"].forEach(function(mix) {
              if (r[mix] !== undefined) {
                cc.mandatory = cc.mandatory || {};
                cc.mandatory[oldname_(mix, key)] = r[mix];
              }
            });
          }
        });
        if (c.advanced) {
          cc.optional = (cc.optional || []).concat(c.advanced);
        }
        return cc;
      };
      var shimConstraints_ = function(constraints, func) {
        if (browserDetails.version >= 61) {
          return func(constraints);
        }
        constraints = JSON.parse(JSON.stringify(constraints));
        if (constraints && typeof constraints.audio === "object") {
          var remap = function(obj, a, b) {
            if (a in obj && !(b in obj)) {
              obj[b] = obj[a];
              delete obj[a];
            }
          };
          constraints = JSON.parse(JSON.stringify(constraints));
          remap(constraints.audio, "autoGainControl", "googAutoGainControl");
          remap(constraints.audio, "noiseSuppression", "googNoiseSuppression");
          constraints.audio = constraintsToChrome_(constraints.audio);
        }
        if (constraints && typeof constraints.video === "object") {
          var face = constraints.video.facingMode;
          face = face && (typeof face === "object" ? face : {ideal:face});
          var getSupportedFacingModeLies = browserDetails.version < 66;
          if (face && (face.exact === "user" || face.exact === "environment" || face.ideal === "user" || face.ideal === "environment") && !(navigator.mediaDevices.getSupportedConstraints && navigator.mediaDevices.getSupportedConstraints().facingMode && !getSupportedFacingModeLies)) {
            delete constraints.video.facingMode;
            var matches;
            if (face.exact === "environment" || face.ideal === "environment") {
              matches = ["back", "rear"];
            } else {
              if (face.exact === "user" || face.ideal === "user") {
                matches = ["front"];
              }
            }
            if (matches) {
              return navigator.mediaDevices.enumerateDevices().then(function(devices) {
                devices = devices.filter(function(d) {
                  return d.kind === "videoinput";
                });
                var dev = devices.find(function(d) {
                  return matches.some(function(match) {
                    return d.label.toLowerCase().indexOf(match) !== -1;
                  });
                });
                if (!dev && devices.length && matches.indexOf("back") !== -1) {
                  dev = devices[devices.length - 1];
                }
                if (dev) {
                  constraints.video.deviceId = face.exact ? {exact:dev.deviceId} : {ideal:dev.deviceId};
                }
                constraints.video = constraintsToChrome_(constraints.video);
                logging("chrome: " + JSON.stringify(constraints));
                return func(constraints);
              });
            }
          }
          constraints.video = constraintsToChrome_(constraints.video);
        }
        logging("chrome: " + JSON.stringify(constraints));
        return func(constraints);
      };
      var shimError_ = function(e) {
        return {name:{PermissionDeniedError:"NotAllowedError", PermissionDismissedError:"NotAllowedError", InvalidStateError:"NotAllowedError", DevicesNotFoundError:"NotFoundError", ConstraintNotSatisfiedError:"OverconstrainedError", TrackStartError:"NotReadableError", MediaDeviceFailedDueToShutdown:"NotAllowedError", MediaDeviceKillSwitchOn:"NotAllowedError", TabCaptureError:"AbortError", ScreenCaptureError:"AbortError", DeviceCaptureError:"AbortError"}[e.name] || e.name, message:e.message, constraint:e.constraintName, 
        toString:function() {
          return this.name + (this.message && ": ") + this.message;
        }};
      };
      var getUserMedia_ = function(constraints, onSuccess, onError) {
        shimConstraints_(constraints, function(c) {
          navigator.webkitGetUserMedia(c, onSuccess, function(e) {
            if (onError) {
              onError(shimError_(e));
            }
          });
        });
      };
      navigator.getUserMedia = getUserMedia_;
      var getUserMediaPromise_ = function(constraints) {
        return new Promise(function(resolve, reject) {
          navigator.getUserMedia(constraints, resolve, reject);
        });
      };
      if (!navigator.mediaDevices) {
        navigator.mediaDevices = {getUserMedia:getUserMediaPromise_, enumerateDevices:function() {
          return new Promise(function(resolve) {
            var kinds = {audio:"audioinput", video:"videoinput"};
            return window.MediaStreamTrack.getSources(function(devices) {
              resolve(devices.map(function(device) {
                return {label:device.label, kind:kinds[device.kind], deviceId:device.id, groupId:""};
              }));
            });
          });
        }, getSupportedConstraints:function() {
          return {deviceId:true, echoCancellation:true, facingMode:true, frameRate:true, height:true, width:true};
        }};
      }
      if (!navigator.mediaDevices.getUserMedia) {
        navigator.mediaDevices.getUserMedia = function(constraints) {
          return getUserMediaPromise_(constraints);
        };
      } else {
        var origGetUserMedia = navigator.mediaDevices.getUserMedia.bind(navigator.mediaDevices);
        navigator.mediaDevices.getUserMedia = function(cs) {
          return shimConstraints_(cs, function(c) {
            return origGetUserMedia(c).then(function(stream) {
              if (c.audio && !stream.getAudioTracks().length || c.video && !stream.getVideoTracks().length) {
                stream.getTracks().forEach(function(track) {
                  track.stop();
                });
                throw new DOMException("", "NotFoundError");
              }
              return stream;
            }, function(e) {
              return Promise.reject(shimError_(e));
            });
          });
        };
      }
      if (typeof navigator.mediaDevices.addEventListener === "undefined") {
        navigator.mediaDevices.addEventListener = function() {
          logging("Dummy mediaDevices.addEventListener called.");
        };
      }
      if (typeof navigator.mediaDevices.removeEventListener === "undefined") {
        navigator.mediaDevices.removeEventListener = function() {
          logging("Dummy mediaDevices.removeEventListener called.");
        };
      }
    };
  }, {"../utils.js":14}], 8:[function(require, module, exports) {
    var SDPUtils = require("sdp");
    var utils = require("./utils");
    module.exports = {shimRTCIceCandidate:function(window) {
      if (window.RTCIceCandidate && "foundation" in window.RTCIceCandidate.prototype) {
        return;
      }
      var NativeRTCIceCandidate = window.RTCIceCandidate;
      window.RTCIceCandidate = function(args) {
        if (typeof args === "object" && args.candidate && args.candidate.indexOf("a=") === 0) {
          args = JSON.parse(JSON.stringify(args));
          args.candidate = args.candidate.substr(2);
        }
        if (args.candidate && args.candidate.length) {
          var nativeCandidate = new NativeRTCIceCandidate(args);
          var parsedCandidate = SDPUtils.parseCandidate(args.candidate);
          var augmentedCandidate = Object.assign(nativeCandidate, parsedCandidate);
          augmentedCandidate.toJSON = function() {
            return {candidate:augmentedCandidate.candidate, sdpMid:augmentedCandidate.sdpMid, sdpMLineIndex:augmentedCandidate.sdpMLineIndex, usernameFragment:augmentedCandidate.usernameFragment};
          };
          return augmentedCandidate;
        }
        return new NativeRTCIceCandidate(args);
      };
      window.RTCIceCandidate.prototype = NativeRTCIceCandidate.prototype;
      utils.wrapPeerConnectionEvent(window, "icecandidate", function(e) {
        if (e.candidate) {
          Object.defineProperty(e, "candidate", {value:new window.RTCIceCandidate(e.candidate), writable:"false"});
        }
        return e;
      });
    }, shimCreateObjectURL:function(window) {
      var URL = window && window.URL;
      if (!(typeof window === "object" && window.HTMLMediaElement && "srcObject" in window.HTMLMediaElement.prototype && URL.createObjectURL && URL.revokeObjectURL)) {
        return undefined;
      }
      var nativeCreateObjectURL = URL.createObjectURL.bind(URL);
      var nativeRevokeObjectURL = URL.revokeObjectURL.bind(URL);
      var streams = new Map, newId = 0;
      URL.createObjectURL = function(stream) {
        if ("getTracks" in stream) {
          var url = "polyblob:" + ++newId;
          streams.set(url, stream);
          utils.deprecated("URL.createObjectURL(stream)", "elem.srcObject = stream");
          return url;
        }
        return nativeCreateObjectURL(stream);
      };
      URL.revokeObjectURL = function(url) {
        nativeRevokeObjectURL(url);
        streams.delete(url);
      };
      var dsc = Object.getOwnPropertyDescriptor(window.HTMLMediaElement.prototype, "src");
      Object.defineProperty(window.HTMLMediaElement.prototype, "src", {get:function() {
        return dsc.get.apply(this);
      }, set:function(url) {
        this.srcObject = streams.get(url) || null;
        return dsc.set.apply(this, [url]);
      }});
      var nativeSetAttribute = window.HTMLMediaElement.prototype.setAttribute;
      window.HTMLMediaElement.prototype.setAttribute = function() {
        if (arguments.length === 2 && ("" + arguments[0]).toLowerCase() === "src") {
          this.srcObject = streams.get(arguments[1]) || null;
        }
        return nativeSetAttribute.apply(this, arguments);
      };
    }, shimMaxMessageSize:function(window) {
      if (window.RTCSctpTransport || !window.RTCPeerConnection) {
        return;
      }
      var browserDetails = utils.detectBrowser(window);
      if (!("sctp" in window.RTCPeerConnection.prototype)) {
        Object.defineProperty(window.RTCPeerConnection.prototype, "sctp", {get:function() {
          return typeof this._sctp === "undefined" ? null : this._sctp;
        }});
      }
      var sctpInDescription = function(description) {
        var sections = SDPUtils.splitSections(description.sdp);
        sections.shift();
        return sections.some(function(mediaSection) {
          var mLine = SDPUtils.parseMLine(mediaSection);
          return mLine && mLine.kind === "application" && mLine.protocol.indexOf("SCTP") !== -1;
        });
      };
      var getRemoteFirefoxVersion = function(description) {
        var match = description.sdp.match(/mozilla...THIS_IS_SDPARTA-(\d+)/);
        if (match === null || match.length < 2) {
          return -1;
        }
        var version = parseInt(match[1], 10);
        return version !== version ? -1 : version;
      };
      var getCanSendMaxMessageSize = function(remoteIsFirefox) {
        var canSendMaxMessageSize = 65536;
        if (browserDetails.browser === "firefox") {
          if (browserDetails.version < 57) {
            if (remoteIsFirefox === -1) {
              canSendMaxMessageSize = 16384;
            } else {
              canSendMaxMessageSize = 2147483637;
            }
          } else {
            canSendMaxMessageSize = browserDetails.version === 57 ? 65535 : 65536;
          }
        }
        return canSendMaxMessageSize;
      };
      var getMaxMessageSize = function(description, remoteIsFirefox) {
        var maxMessageSize = 65536;
        if (browserDetails.browser === "firefox" && browserDetails.version === 57) {
          maxMessageSize = 65535;
        }
        var match = SDPUtils.matchPrefix(description.sdp, "a=max-message-size:");
        if (match.length > 0) {
          maxMessageSize = parseInt(match[0].substr(19), 10);
        } else {
          if (browserDetails.browser === "firefox" && remoteIsFirefox !== -1) {
            maxMessageSize = 2147483637;
          }
        }
        return maxMessageSize;
      };
      var origSetRemoteDescription = window.RTCPeerConnection.prototype.setRemoteDescription;
      window.RTCPeerConnection.prototype.setRemoteDescription = function() {
        var pc = this;
        pc._sctp = null;
        if (sctpInDescription(arguments[0])) {
          var isFirefox = getRemoteFirefoxVersion(arguments[0]);
          var canSendMMS = getCanSendMaxMessageSize(isFirefox);
          var remoteMMS = getMaxMessageSize(arguments[0], isFirefox);
          var maxMessageSize;
          if (canSendMMS === 0 && remoteMMS === 0) {
            maxMessageSize = Number.POSITIVE_INFINITY;
          } else {
            if (canSendMMS === 0 || remoteMMS === 0) {
              maxMessageSize = Math.max(canSendMMS, remoteMMS);
            } else {
              maxMessageSize = Math.min(canSendMMS, remoteMMS);
            }
          }
          var sctp = {};
          Object.defineProperty(sctp, "maxMessageSize", {get:function() {
            return maxMessageSize;
          }});
          pc._sctp = sctp;
        }
        return origSetRemoteDescription.apply(pc, arguments);
      };
    }, shimSendThrowTypeError:function(window) {
      if (!window.RTCPeerConnection) {
        return;
      }
      var origCreateDataChannel = window.RTCPeerConnection.prototype.createDataChannel;
      window.RTCPeerConnection.prototype.createDataChannel = function() {
        var pc = this;
        var dataChannel = origCreateDataChannel.apply(pc, arguments);
        var origDataChannelSend = dataChannel.send;
        dataChannel.send = function() {
          var dc = this;
          var data = arguments[0];
          var length = data.length || data.size || data.byteLength;
          if (length > pc.sctp.maxMessageSize) {
            throw new DOMException("Message too large (can send a maximum of " + pc.sctp.maxMessageSize + " bytes)", "TypeError");
          }
          return origDataChannelSend.apply(dc, arguments);
        };
        return dataChannel;
      };
    }};
  }, {"./utils":14, "sdp":3}], 9:[function(require, module, exports) {
    var utils = require("../utils");
    var shimRTCPeerConnection = require("rtcpeerconnection-shim");
    module.exports = {shimGetUserMedia:require("./getusermedia"), shimPeerConnection:function(window) {
      var browserDetails = utils.detectBrowser(window);
      if (window.RTCIceGatherer) {
        if (!window.RTCIceCandidate) {
          window.RTCIceCandidate = function(args) {
            return args;
          };
        }
        if (!window.RTCSessionDescription) {
          window.RTCSessionDescription = function(args) {
            return args;
          };
        }
        if (browserDetails.version < 15025) {
          var origMSTEnabled = Object.getOwnPropertyDescriptor(window.MediaStreamTrack.prototype, "enabled");
          Object.defineProperty(window.MediaStreamTrack.prototype, "enabled", {set:function(value) {
            origMSTEnabled.set.call(this, value);
            var ev = new Event("enabled");
            ev.enabled = value;
            this.dispatchEvent(ev);
          }});
        }
      }
      if (window.RTCRtpSender && !("dtmf" in window.RTCRtpSender.prototype)) {
        Object.defineProperty(window.RTCRtpSender.prototype, "dtmf", {get:function() {
          if (this._dtmf === undefined) {
            if (this.track.kind === "audio") {
              this._dtmf = new window.RTCDtmfSender(this);
            } else {
              if (this.track.kind === "video") {
                this._dtmf = null;
              }
            }
          }
          return this._dtmf;
        }});
      }
      window.RTCPeerConnection = shimRTCPeerConnection(window, browserDetails.version);
    }, shimReplaceTrack:function(window) {
      if (window.RTCRtpSender && !("replaceTrack" in window.RTCRtpSender.prototype)) {
        window.RTCRtpSender.prototype.replaceTrack = window.RTCRtpSender.prototype.setTrack;
      }
    }};
  }, {"../utils":14, "./getusermedia":10, "rtcpeerconnection-shim":2}], 10:[function(require, module, exports) {
    module.exports = function(window) {
      var navigator = window && window.navigator;
      var shimError_ = function(e) {
        return {name:{PermissionDeniedError:"NotAllowedError"}[e.name] || e.name, message:e.message, constraint:e.constraint, toString:function() {
          return this.name;
        }};
      };
      var origGetUserMedia = navigator.mediaDevices.getUserMedia.bind(navigator.mediaDevices);
      navigator.mediaDevices.getUserMedia = function(c) {
        return origGetUserMedia(c).catch(function(e) {
          return Promise.reject(shimError_(e));
        });
      };
    };
  }, {}], 11:[function(require, module, exports) {
    var utils = require("../utils");
    module.exports = {shimGetUserMedia:require("./getusermedia"), shimOnTrack:function(window) {
      if (typeof window === "object" && window.RTCPeerConnection && !("ontrack" in window.RTCPeerConnection.prototype)) {
        Object.defineProperty(window.RTCPeerConnection.prototype, "ontrack", {get:function() {
          return this._ontrack;
        }, set:function(f) {
          if (this._ontrack) {
            this.removeEventListener("track", this._ontrack);
            this.removeEventListener("addstream", this._ontrackpoly);
          }
          this.addEventListener("track", this._ontrack = f);
          this.addEventListener("addstream", this._ontrackpoly = function(e) {
            e.stream.getTracks().forEach(function(track) {
              var event = new Event("track");
              event.track = track;
              event.receiver = {track:track};
              event.transceiver = {receiver:event.receiver};
              event.streams = [e.stream];
              this.dispatchEvent(event);
            }.bind(this));
          }.bind(this));
        }});
      }
      if (typeof window === "object" && window.RTCTrackEvent && "receiver" in window.RTCTrackEvent.prototype && !("transceiver" in window.RTCTrackEvent.prototype)) {
        Object.defineProperty(window.RTCTrackEvent.prototype, "transceiver", {get:function() {
          return {receiver:this.receiver};
        }});
      }
    }, shimSourceObject:function(window) {
      if (typeof window === "object") {
        if (window.HTMLMediaElement && !("srcObject" in window.HTMLMediaElement.prototype)) {
          Object.defineProperty(window.HTMLMediaElement.prototype, "srcObject", {get:function() {
            return this.mozSrcObject;
          }, set:function(stream) {
            this.mozSrcObject = stream;
          }});
        }
      }
    }, shimPeerConnection:function(window) {
      var browserDetails = utils.detectBrowser(window);
      if (typeof window !== "object" || !(window.RTCPeerConnection || window.mozRTCPeerConnection)) {
        return;
      }
      if (!window.RTCPeerConnection) {
        window.RTCPeerConnection = function(pcConfig, pcConstraints) {
          if (browserDetails.version < 38) {
            if (pcConfig && pcConfig.iceServers) {
              var newIceServers = [];
              for (var i = 0; i < pcConfig.iceServers.length; i++) {
                var server = pcConfig.iceServers[i];
                if (server.hasOwnProperty("urls")) {
                  for (var j = 0; j < server.urls.length; j++) {
                    var newServer = {url:server.urls[j]};
                    if (server.urls[j].indexOf("turn") === 0) {
                      newServer.username = server.username;
                      newServer.credential = server.credential;
                    }
                    newIceServers.push(newServer);
                  }
                } else {
                  newIceServers.push(pcConfig.iceServers[i]);
                }
              }
              pcConfig.iceServers = newIceServers;
            }
          }
          return new window.mozRTCPeerConnection(pcConfig, pcConstraints);
        };
        window.RTCPeerConnection.prototype = window.mozRTCPeerConnection.prototype;
        if (window.mozRTCPeerConnection.generateCertificate) {
          Object.defineProperty(window.RTCPeerConnection, "generateCertificate", {get:function() {
            return window.mozRTCPeerConnection.generateCertificate;
          }});
        }
        window.RTCSessionDescription = window.mozRTCSessionDescription;
        window.RTCIceCandidate = window.mozRTCIceCandidate;
      }
      ["setLocalDescription", "setRemoteDescription", "addIceCandidate"].forEach(function(method) {
        var nativeMethod = window.RTCPeerConnection.prototype[method];
        window.RTCPeerConnection.prototype[method] = function() {
          arguments[0] = new (method === "addIceCandidate" ? window.RTCIceCandidate : window.RTCSessionDescription)(arguments[0]);
          return nativeMethod.apply(this, arguments);
        };
      });
      var nativeAddIceCandidate = window.RTCPeerConnection.prototype.addIceCandidate;
      window.RTCPeerConnection.prototype.addIceCandidate = function() {
        if (!arguments[0]) {
          if (arguments[1]) {
            arguments[1].apply(null);
          }
          return Promise.resolve();
        }
        return nativeAddIceCandidate.apply(this, arguments);
      };
      var makeMapStats = function(stats) {
        var map = new Map;
        Object.keys(stats).forEach(function(key) {
          map.set(key, stats[key]);
          map[key] = stats[key];
        });
        return map;
      };
      var modernStatsTypes = {inboundrtp:"inbound-rtp", outboundrtp:"outbound-rtp", candidatepair:"candidate-pair", localcandidate:"local-candidate", remotecandidate:"remote-candidate"};
      var nativeGetStats = window.RTCPeerConnection.prototype.getStats;
      window.RTCPeerConnection.prototype.getStats = function(selector, onSucc, onErr) {
        return nativeGetStats.apply(this, [selector || null]).then(function(stats) {
          if (browserDetails.version < 48) {
            stats = makeMapStats(stats);
          }
          if (browserDetails.version < 53 && !onSucc) {
            try {
              stats.forEach(function(stat) {
                stat.type = modernStatsTypes[stat.type] || stat.type;
              });
            } catch (e) {
              if (e.name !== "TypeError") {
                throw e;
              }
              stats.forEach(function(stat, i) {
                stats.set(i, Object.assign({}, stat, {type:modernStatsTypes[stat.type] || stat.type}));
              });
            }
          }
          return stats;
        }).then(onSucc, onErr);
      };
    }, shimRemoveStream:function(window) {
      if (!window.RTCPeerConnection || "removeStream" in window.RTCPeerConnection.prototype) {
        return;
      }
      window.RTCPeerConnection.prototype.removeStream = function(stream) {
        var pc = this;
        utils.deprecated("removeStream", "removeTrack");
        this.getSenders().forEach(function(sender) {
          if (sender.track && stream.getTracks().indexOf(sender.track) !== -1) {
            pc.removeTrack(sender);
          }
        });
      };
    }};
  }, {"../utils":14, "./getusermedia":12}], 12:[function(require, module, exports) {
    var utils = require("../utils");
    var logging = utils.log;
    module.exports = function(window) {
      var browserDetails = utils.detectBrowser(window);
      var navigator = window && window.navigator;
      var MediaStreamTrack = window && window.MediaStreamTrack;
      var shimError_ = function(e) {
        return {name:{InternalError:"NotReadableError", NotSupportedError:"TypeError", PermissionDeniedError:"NotAllowedError", SecurityError:"NotAllowedError"}[e.name] || e.name, message:{"The operation is insecure.":"The request is not allowed by the " + "user agent or the platform in the current context."}[e.message] || e.message, constraint:e.constraint, toString:function() {
          return this.name + (this.message && ": ") + this.message;
        }};
      };
      var getUserMedia_ = function(constraints, onSuccess, onError) {
        var constraintsToFF37_ = function(c) {
          if (typeof c !== "object" || c.require) {
            return c;
          }
          var require = [];
          Object.keys(c).forEach(function(key) {
            if (key === "require" || key === "advanced" || key === "mediaSource") {
              return;
            }
            var r = c[key] = typeof c[key] === "object" ? c[key] : {ideal:c[key]};
            if (r.min !== undefined || r.max !== undefined || r.exact !== undefined) {
              require.push(key);
            }
            if (r.exact !== undefined) {
              if (typeof r.exact === "number") {
                r.min = r.max = r.exact;
              } else {
                c[key] = r.exact;
              }
              delete r.exact;
            }
            if (r.ideal !== undefined) {
              c.advanced = c.advanced || [];
              var oc = {};
              if (typeof r.ideal === "number") {
                oc[key] = {min:r.ideal, max:r.ideal};
              } else {
                oc[key] = r.ideal;
              }
              c.advanced.push(oc);
              delete r.ideal;
              if (!Object.keys(r).length) {
                delete c[key];
              }
            }
          });
          if (require.length) {
            c.require = require;
          }
          return c;
        };
        constraints = JSON.parse(JSON.stringify(constraints));
        if (browserDetails.version < 38) {
          logging("spec: " + JSON.stringify(constraints));
          if (constraints.audio) {
            constraints.audio = constraintsToFF37_(constraints.audio);
          }
          if (constraints.video) {
            constraints.video = constraintsToFF37_(constraints.video);
          }
          logging("ff37: " + JSON.stringify(constraints));
        }
        return navigator.mozGetUserMedia(constraints, onSuccess, function(e) {
          onError(shimError_(e));
        });
      };
      var getUserMediaPromise_ = function(constraints) {
        return new Promise(function(resolve, reject) {
          getUserMedia_(constraints, resolve, reject);
        });
      };
      if (!navigator.mediaDevices) {
        navigator.mediaDevices = {getUserMedia:getUserMediaPromise_, addEventListener:function() {
        }, removeEventListener:function() {
        }};
      }
      navigator.mediaDevices.enumerateDevices = navigator.mediaDevices.enumerateDevices || function() {
        return new Promise(function(resolve) {
          var infos = [{kind:"audioinput", deviceId:"default", label:"", groupId:""}, {kind:"videoinput", deviceId:"default", label:"", groupId:""}];
          resolve(infos);
        });
      };
      if (browserDetails.version < 41) {
        var orgEnumerateDevices = navigator.mediaDevices.enumerateDevices.bind(navigator.mediaDevices);
        navigator.mediaDevices.enumerateDevices = function() {
          return orgEnumerateDevices().then(undefined, function(e) {
            if (e.name === "NotFoundError") {
              return [];
            }
            throw e;
          });
        };
      }
      if (browserDetails.version < 49) {
        var origGetUserMedia = navigator.mediaDevices.getUserMedia.bind(navigator.mediaDevices);
        navigator.mediaDevices.getUserMedia = function(c) {
          return origGetUserMedia(c).then(function(stream) {
            if (c.audio && !stream.getAudioTracks().length || c.video && !stream.getVideoTracks().length) {
              stream.getTracks().forEach(function(track) {
                track.stop();
              });
              throw new DOMException("The object can not be found here.", "NotFoundError");
            }
            return stream;
          }, function(e) {
            return Promise.reject(shimError_(e));
          });
        };
      }
      if (!(browserDetails.version > 55 && "autoGainControl" in navigator.mediaDevices.getSupportedConstraints())) {
        var remap = function(obj, a, b) {
          if (a in obj && !(b in obj)) {
            obj[b] = obj[a];
            delete obj[a];
          }
        };
        var nativeGetUserMedia = navigator.mediaDevices.getUserMedia.bind(navigator.mediaDevices);
        navigator.mediaDevices.getUserMedia = function(c) {
          if (typeof c === "object" && typeof c.audio === "object") {
            c = JSON.parse(JSON.stringify(c));
            remap(c.audio, "autoGainControl", "mozAutoGainControl");
            remap(c.audio, "noiseSuppression", "mozNoiseSuppression");
          }
          return nativeGetUserMedia(c);
        };
        if (MediaStreamTrack && MediaStreamTrack.prototype.getSettings) {
          var nativeGetSettings = MediaStreamTrack.prototype.getSettings;
          MediaStreamTrack.prototype.getSettings = function() {
            var obj = nativeGetSettings.apply(this, arguments);
            remap(obj, "mozAutoGainControl", "autoGainControl");
            remap(obj, "mozNoiseSuppression", "noiseSuppression");
            return obj;
          };
        }
        if (MediaStreamTrack && MediaStreamTrack.prototype.applyConstraints) {
          var nativeApplyConstraints = MediaStreamTrack.prototype.applyConstraints;
          MediaStreamTrack.prototype.applyConstraints = function(c) {
            if (this.kind === "audio" && typeof c === "object") {
              c = JSON.parse(JSON.stringify(c));
              remap(c, "autoGainControl", "mozAutoGainControl");
              remap(c, "noiseSuppression", "mozNoiseSuppression");
            }
            return nativeApplyConstraints.apply(this, [c]);
          };
        }
      }
      navigator.getUserMedia = function(constraints, onSuccess, onError) {
        if (browserDetails.version < 44) {
          return getUserMedia_(constraints, onSuccess, onError);
        }
        utils.deprecated("navigator.getUserMedia", "navigator.mediaDevices.getUserMedia");
        navigator.mediaDevices.getUserMedia(constraints).then(onSuccess, onError);
      };
    };
  }, {"../utils":14}], 13:[function(require, module, exports) {
    var utils = require("../utils");
    module.exports = {shimLocalStreamsAPI:function(window) {
      if (typeof window !== "object" || !window.RTCPeerConnection) {
        return;
      }
      if (!("getLocalStreams" in window.RTCPeerConnection.prototype)) {
        window.RTCPeerConnection.prototype.getLocalStreams = function() {
          if (!this._localStreams) {
            this._localStreams = [];
          }
          return this._localStreams;
        };
      }
      if (!("getStreamById" in window.RTCPeerConnection.prototype)) {
        window.RTCPeerConnection.prototype.getStreamById = function(id) {
          var result = null;
          if (this._localStreams) {
            this._localStreams.forEach(function(stream) {
              if (stream.id === id) {
                result = stream;
              }
            });
          }
          if (this._remoteStreams) {
            this._remoteStreams.forEach(function(stream) {
              if (stream.id === id) {
                result = stream;
              }
            });
          }
          return result;
        };
      }
      if (!("addStream" in window.RTCPeerConnection.prototype)) {
        var _addTrack = window.RTCPeerConnection.prototype.addTrack;
        window.RTCPeerConnection.prototype.addStream = function(stream) {
          if (!this._localStreams) {
            this._localStreams = [];
          }
          if (this._localStreams.indexOf(stream) === -1) {
            this._localStreams.push(stream);
          }
          var pc = this;
          stream.getTracks().forEach(function(track) {
            _addTrack.call(pc, track, stream);
          });
        };
        window.RTCPeerConnection.prototype.addTrack = function(track, stream) {
          if (stream) {
            if (!this._localStreams) {
              this._localStreams = [stream];
            } else {
              if (this._localStreams.indexOf(stream) === -1) {
                this._localStreams.push(stream);
              }
            }
          }
          return _addTrack.call(this, track, stream);
        };
      }
      if (!("removeStream" in window.RTCPeerConnection.prototype)) {
        window.RTCPeerConnection.prototype.removeStream = function(stream) {
          if (!this._localStreams) {
            this._localStreams = [];
          }
          var index = this._localStreams.indexOf(stream);
          if (index === -1) {
            return;
          }
          this._localStreams.splice(index, 1);
          var pc = this;
          var tracks = stream.getTracks();
          this.getSenders().forEach(function(sender) {
            if (tracks.indexOf(sender.track) !== -1) {
              pc.removeTrack(sender);
            }
          });
        };
      }
    }, shimRemoteStreamsAPI:function(window) {
      if (typeof window !== "object" || !window.RTCPeerConnection) {
        return;
      }
      if (!("getRemoteStreams" in window.RTCPeerConnection.prototype)) {
        window.RTCPeerConnection.prototype.getRemoteStreams = function() {
          return this._remoteStreams ? this._remoteStreams : [];
        };
      }
      if (!("onaddstream" in window.RTCPeerConnection.prototype)) {
        Object.defineProperty(window.RTCPeerConnection.prototype, "onaddstream", {get:function() {
          return this._onaddstream;
        }, set:function(f) {
          var pc = this;
          if (this._onaddstream) {
            this.removeEventListener("addstream", this._onaddstream);
            this.removeEventListener("track", this._onaddstreampoly);
          }
          this.addEventListener("addstream", this._onaddstream = f);
          this.addEventListener("track", this._onaddstreampoly = function(e) {
            e.streams.forEach(function(stream) {
              if (!pc._remoteStreams) {
                pc._remoteStreams = [];
              }
              if (pc._remoteStreams.indexOf(stream) >= 0) {
                return;
              }
              pc._remoteStreams.push(stream);
              var event = new Event("addstream");
              event.stream = stream;
              pc.dispatchEvent(event);
            });
          });
        }});
      }
    }, shimCallbacksAPI:function(window) {
      if (typeof window !== "object" || !window.RTCPeerConnection) {
        return;
      }
      var prototype = window.RTCPeerConnection.prototype;
      var createOffer = prototype.createOffer;
      var createAnswer = prototype.createAnswer;
      var setLocalDescription = prototype.setLocalDescription;
      var setRemoteDescription = prototype.setRemoteDescription;
      var addIceCandidate = prototype.addIceCandidate;
      prototype.createOffer = function(successCallback, failureCallback) {
        var options = arguments.length >= 2 ? arguments[2] : arguments[0];
        var promise = createOffer.apply(this, [options]);
        if (!failureCallback) {
          return promise;
        }
        promise.then(successCallback, failureCallback);
        return Promise.resolve();
      };
      prototype.createAnswer = function(successCallback, failureCallback) {
        var options = arguments.length >= 2 ? arguments[2] : arguments[0];
        var promise = createAnswer.apply(this, [options]);
        if (!failureCallback) {
          return promise;
        }
        promise.then(successCallback, failureCallback);
        return Promise.resolve();
      };
      var withCallback = function(description, successCallback, failureCallback) {
        var promise = setLocalDescription.apply(this, [description]);
        if (!failureCallback) {
          return promise;
        }
        promise.then(successCallback, failureCallback);
        return Promise.resolve();
      };
      prototype.setLocalDescription = withCallback;
      withCallback = function(description, successCallback, failureCallback) {
        var promise = setRemoteDescription.apply(this, [description]);
        if (!failureCallback) {
          return promise;
        }
        promise.then(successCallback, failureCallback);
        return Promise.resolve();
      };
      prototype.setRemoteDescription = withCallback;
      withCallback = function(candidate, successCallback, failureCallback) {
        var promise = addIceCandidate.apply(this, [candidate]);
        if (!failureCallback) {
          return promise;
        }
        promise.then(successCallback, failureCallback);
        return Promise.resolve();
      };
      prototype.addIceCandidate = withCallback;
    }, shimGetUserMedia:function(window) {
      var navigator = window && window.navigator;
      if (!navigator.getUserMedia) {
        if (navigator.webkitGetUserMedia) {
          navigator.getUserMedia = navigator.webkitGetUserMedia.bind(navigator);
        } else {
          if (navigator.mediaDevices && navigator.mediaDevices.getUserMedia) {
            navigator.getUserMedia = function(constraints, cb, errcb) {
              navigator.mediaDevices.getUserMedia(constraints).then(cb, errcb);
            }.bind(navigator);
          }
        }
      }
    }, shimRTCIceServerUrls:function(window) {
      var OrigPeerConnection = window.RTCPeerConnection;
      window.RTCPeerConnection = function(pcConfig, pcConstraints) {
        if (pcConfig && pcConfig.iceServers) {
          var newIceServers = [];
          for (var i = 0; i < pcConfig.iceServers.length; i++) {
            var server = pcConfig.iceServers[i];
            if (!server.hasOwnProperty("urls") && server.hasOwnProperty("url")) {
              utils.deprecated("RTCIceServer.url", "RTCIceServer.urls");
              server = JSON.parse(JSON.stringify(server));
              server.urls = server.url;
              delete server.url;
              newIceServers.push(server);
            } else {
              newIceServers.push(pcConfig.iceServers[i]);
            }
          }
          pcConfig.iceServers = newIceServers;
        }
        return new OrigPeerConnection(pcConfig, pcConstraints);
      };
      window.RTCPeerConnection.prototype = OrigPeerConnection.prototype;
      if ("generateCertificate" in window.RTCPeerConnection) {
        Object.defineProperty(window.RTCPeerConnection, "generateCertificate", {get:function() {
          return OrigPeerConnection.generateCertificate;
        }});
      }
    }, shimTrackEventTransceiver:function(window) {
      if (typeof window === "object" && window.RTCPeerConnection && "receiver" in window.RTCTrackEvent.prototype && !window.RTCTransceiver) {
        Object.defineProperty(window.RTCTrackEvent.prototype, "transceiver", {get:function() {
          return {receiver:this.receiver};
        }});
      }
    }, shimCreateOfferLegacy:function(window) {
      var origCreateOffer = window.RTCPeerConnection.prototype.createOffer;
      window.RTCPeerConnection.prototype.createOffer = function(offerOptions) {
        var pc = this;
        if (offerOptions) {
          var audioTransceiver = pc.getTransceivers().find(function(transceiver) {
            return transceiver.sender.track && transceiver.sender.track.kind === "audio";
          });
          if (offerOptions.offerToReceiveAudio === false && audioTransceiver) {
            if (audioTransceiver.direction === "sendrecv") {
              if (audioTransceiver.setDirection) {
                audioTransceiver.setDirection("sendonly");
              } else {
                audioTransceiver.direction = "sendonly";
              }
            } else {
              if (audioTransceiver.direction === "recvonly") {
                if (audioTransceiver.setDirection) {
                  audioTransceiver.setDirection("inactive");
                } else {
                  audioTransceiver.direction = "inactive";
                }
              }
            }
          } else {
            if (offerOptions.offerToReceiveAudio === true && !audioTransceiver) {
              pc.addTransceiver("audio");
            }
          }
          var videoTransceiver = pc.getTransceivers().find(function(transceiver) {
            return transceiver.sender.track && transceiver.sender.track.kind === "video";
          });
          if (offerOptions.offerToReceiveVideo === false && videoTransceiver) {
            if (videoTransceiver.direction === "sendrecv") {
              videoTransceiver.setDirection("sendonly");
            } else {
              if (videoTransceiver.direction === "recvonly") {
                videoTransceiver.setDirection("inactive");
              }
            }
          } else {
            if (offerOptions.offerToReceiveVideo === true && !videoTransceiver) {
              pc.addTransceiver("video");
            }
          }
        }
        return origCreateOffer.apply(pc, arguments);
      };
    }};
  }, {"../utils":14}], 14:[function(require, module, exports) {
    var logDisabled_ = true;
    var deprecationWarnings_ = true;
    function extractVersion(uastring, expr, pos) {
      var match = uastring.match(expr);
      return match && match.length >= pos && parseInt(match[pos], 10);
    }
    function wrapPeerConnectionEvent(window, eventNameToWrap, wrapper) {
      if (!window.RTCPeerConnection) {
        return;
      }
      var proto = window.RTCPeerConnection.prototype;
      var nativeAddEventListener = proto.addEventListener;
      proto.addEventListener = function(nativeEventName, cb) {
        if (nativeEventName !== eventNameToWrap) {
          return nativeAddEventListener.apply(this, arguments);
        }
        var wrappedCallback = function(e) {
          cb(wrapper(e));
        };
        this._eventMap = this._eventMap || {};
        this._eventMap[cb] = wrappedCallback;
        return nativeAddEventListener.apply(this, [nativeEventName, wrappedCallback]);
      };
      var nativeRemoveEventListener = proto.removeEventListener;
      proto.removeEventListener = function(nativeEventName, cb) {
        if (nativeEventName !== eventNameToWrap || !this._eventMap || !this._eventMap[cb]) {
          return nativeRemoveEventListener.apply(this, arguments);
        }
        var unwrappedCb = this._eventMap[cb];
        delete this._eventMap[cb];
        return nativeRemoveEventListener.apply(this, [nativeEventName, unwrappedCb]);
      };
      Object.defineProperty(proto, "on" + eventNameToWrap, {get:function() {
        return this["_on" + eventNameToWrap];
      }, set:function(cb) {
        if (this["_on" + eventNameToWrap]) {
          this.removeEventListener(eventNameToWrap, this["_on" + eventNameToWrap]);
          delete this["_on" + eventNameToWrap];
        }
        if (cb) {
          this.addEventListener(eventNameToWrap, this["_on" + eventNameToWrap] = cb);
        }
      }});
    }
    module.exports = {extractVersion:extractVersion, wrapPeerConnectionEvent:wrapPeerConnectionEvent, disableLog:function(bool) {
      if (typeof bool !== "boolean") {
        return new Error("Argument type: " + typeof bool + ". Please use a boolean.");
      }
      logDisabled_ = bool;
      return bool ? "adapter.js logging disabled" : "adapter.js logging enabled";
    }, disableWarnings:function(bool) {
      if (typeof bool !== "boolean") {
        return new Error("Argument type: " + typeof bool + ". Please use a boolean.");
      }
      deprecationWarnings_ = !bool;
      return "adapter.js deprecation warnings " + (bool ? "disabled" : "enabled");
    }, log:function() {
      if (typeof window === "object") {
        if (logDisabled_) {
          return;
        }
        if (typeof console !== "undefined" && typeof console.log === "function") {
          console.log.apply(console, arguments);
        }
      }
    }, deprecated:function(oldMethod, newMethod) {
      if (!deprecationWarnings_) {
        return;
      }
      console.warn(oldMethod + " is deprecated, please use " + newMethod + " instead.");
    }, detectBrowser:function(window) {
      var navigator = window && window.navigator;
      var result = {};
      result.browser = null;
      result.version = null;
      if (typeof window === "undefined" || !window.navigator) {
        result.browser = "Not a browser.";
        return result;
      }
      if (navigator.mozGetUserMedia) {
        result.browser = "firefox";
        result.version = extractVersion(navigator.userAgent, /Firefox\/(\d+)\./, 1);
      } else {
        if (navigator.webkitGetUserMedia) {
          result.browser = "chrome";
          result.version = extractVersion(navigator.userAgent, /Chrom(e|ium)\/(\d+)\./, 2);
        } else {
          if (navigator.mediaDevices && navigator.userAgent.match(/Edge\/(\d+).(\d+)$/)) {
            result.browser = "edge";
            result.version = extractVersion(navigator.userAgent, /Edge\/(\d+).(\d+)$/, 2);
          } else {
            if (window.RTCPeerConnection && navigator.userAgent.match(/AppleWebKit\/(\d+)\./)) {
              result.browser = "safari";
              result.version = extractVersion(navigator.userAgent, /AppleWebKit\/(\d+)\./, 1);
            } else {
              result.browser = "Not a supported browser.";
              return result;
            }
          }
        }
      }
      return result;
    }};
  }, {}]}, {}, [4])(4);
});
var Analytics = function(roomServer) {
  this.analyticsPath_ = roomServer + "/a/";
};
Analytics.EventObject_ = {};
Analytics.prototype.reportEvent = function(eventType, roomId, flowId) {
  var eventObj = {};
  eventObj[enums.RequestField.EventField.EVENT_TYPE] = eventType;
  eventObj[enums.RequestField.EventField.EVENT_TIME_MS] = Date.now();
  if (roomId) {
    eventObj[enums.RequestField.EventField.ROOM_ID] = roomId;
  }
  if (flowId) {
    eventObj[enums.RequestField.EventField.FLOW_ID] = flowId;
  }
  this.sendEventRequest_(eventObj);
};
Analytics.prototype.sendEventRequest_ = function(eventObj) {
  var request = {};
  request[enums.RequestField.TYPE] = enums.RequestField.MessageType.EVENT;
  request[enums.RequestField.REQUEST_TIME_MS] = Date.now();
  request[enums.RequestField.EVENT] = eventObj;
  sendAsyncUrlRequest("POST", this.analyticsPath_, JSON.stringify(request)).then(function() {
  }.bind(this), function(error) {
    trace("Failed to send event request: " + error.message);
  }.bind(this));
};
var enums = {"EventType":{"ICE_CONNECTION_STATE_CONNECTED":3, "ROOM_SIZE_2":2}, "RequestField":{"MessageType":{"EVENT":"event"}, "CLIENT_TYPE":"client_type", "EventField":{"EVENT_TIME_MS":"event_time_ms", "ROOM_ID":"room_id", "EVENT_TYPE":"event_type", "FLOW_ID":"flow_id"}, "TYPE":"type", "EVENT":"event", "REQUEST_TIME_MS":"request_time_ms"}, "ClientType":{"UNKNOWN":0, "ANDROID":4, "DESKTOP":2, "IOS":3, "JS":1}};
var remoteVideo = $("#remote-video");
var UI_CONSTANTS = {confirmJoinButton:"#confirm-join-button", confirmJoinDiv:"#confirm-join-div", confirmJoinRoomSpan:"#confirm-join-room-span", fullscreenSvg:"#fullscreen", hangupSvg:"#hangup", icons:"#icons", infoDiv:"#info-div", localVideo:"#local-video", miniVideo:"#mini-video", muteAudioSvg:"#mute-audio", muteVideoSvg:"#mute-video", newRoomButton:"#new-room-button", newRoomLink:"#new-room-link", privacyLinks:"#privacy", remoteVideo:"#remote-video", rejoinButton:"#rejoin-button", rejoinDiv:"#rejoin-div", 
rejoinLink:"#rejoin-link", roomLinkHref:"#room-link-href", roomSelectionDiv:"#room-selection", roomSelectionInput:"#room-id-input", roomSelectionInputLabel:"#room-id-input-label", roomSelectionJoinButton:"#join-button", roomSelectionRandomButton:"#random-button", roomSelectionRecentList:"#recent-rooms-list", sharingDiv:"#sharing-div", statusDiv:"#status-div", videosDiv:"#videos"};
var AppController = function(loadingParams) {
  trace("Initializing; server= " + loadingParams.roomServer + ".");
  trace("Initializing; room=" + loadingParams.roomId + ".");
  this.hangupSvg_ = $(UI_CONSTANTS.hangupSvg);
  this.icons_ = $(UI_CONSTANTS.icons);
  this.localVideo_ = $(UI_CONSTANTS.localVideo);
  this.miniVideo_ = $(UI_CONSTANTS.miniVideo);
  this.sharingDiv_ = $(UI_CONSTANTS.sharingDiv);
  this.statusDiv_ = $(UI_CONSTANTS.statusDiv);
  this.remoteVideo_ = $(UI_CONSTANTS.remoteVideo);
  this.videosDiv_ = $(UI_CONSTANTS.videosDiv);
  this.roomLinkHref_ = $(UI_CONSTANTS.roomLinkHref);
  this.rejoinDiv_ = $(UI_CONSTANTS.rejoinDiv);
  this.rejoinLink_ = $(UI_CONSTANTS.rejoinLink);
  this.newRoomLink_ = $(UI_CONSTANTS.newRoomLink);
  this.rejoinButton_ = $(UI_CONSTANTS.rejoinButton);
  this.newRoomButton_ = $(UI_CONSTANTS.newRoomButton);
  this.newRoomButton_.addEventListener("click", this.onNewRoomClick_.bind(this), false);
  this.rejoinButton_.addEventListener("click", this.onRejoinClick_.bind(this), false);
  this.muteAudioIconSet_ = new AppController.IconSet_(UI_CONSTANTS.muteAudioSvg);
  this.muteVideoIconSet_ = new AppController.IconSet_(UI_CONSTANTS.muteVideoSvg);
  this.fullscreenIconSet_ = new AppController.IconSet_(UI_CONSTANTS.fullscreenSvg);
  this.loadingParams_ = loadingParams;
  this.loadUrlParams_();
  var paramsPromise = Promise.resolve({});
  if (this.loadingParams_.paramsFunction) {
    paramsPromise = this.loadingParams_.paramsFunction();
  }
  Promise.resolve(paramsPromise).then(function(newParams) {
    if (newParams) {
      Object.keys(newParams).forEach(function(key) {
        this.loadingParams_[key] = newParams[key];
      }.bind(this));
    }
    this.roomLink_ = "";
    this.roomSelection_ = null;
    this.localStream_ = null;
    this.remoteVideoResetTimer_ = null;
    if (this.loadingParams_.roomId) {
      this.createCall_();
      if (!RoomSelection.matchRandomRoomPattern(this.loadingParams_.roomId)) {
        $(UI_CONSTANTS.confirmJoinRoomSpan).textContent = ' "' + this.loadingParams_.roomId + '"';
      }
      var confirmJoinDiv = $(UI_CONSTANTS.confirmJoinDiv);
      this.show_(confirmJoinDiv);
      $(UI_CONSTANTS.confirmJoinButton).onclick = function() {
        this.hide_(confirmJoinDiv);
        var recentlyUsedList = new RoomSelection.RecentlyUsedList;
        recentlyUsedList.pushRecentRoom(this.loadingParams_.roomId);
        this.finishCallSetup_(this.loadingParams_.roomId);
      }.bind(this);
      if (this.loadingParams_.bypassJoinConfirmation) {
        $(UI_CONSTANTS.confirmJoinButton).onclick();
      }
    } else {
      this.showRoomSelection_();
    }
  }.bind(this)).catch(function(error) {
    trace("Error initializing: " + error.message);
  }.bind(this));
};
AppController.prototype.createCall_ = function() {
  var privacyLinks = $(UI_CONSTANTS.privacyLinks);
  this.hide_(privacyLinks);
  this.call_ = new Call(this.loadingParams_);
  this.infoBox_ = new InfoBox($(UI_CONSTANTS.infoDiv), this.call_, this.loadingParams_.versionInfo);
  var roomErrors = this.loadingParams_.errorMessages;
  var roomWarnings = this.loadingParams_.warningMessages;
  if (roomErrors && roomErrors.length > 0) {
    for (var i = 0; i < roomErrors.length; ++i) {
      this.infoBox_.pushErrorMessage(roomErrors[i]);
    }
    return;
  } else {
    if (roomWarnings && roomWarnings.length > 0) {
      for (var j = 0; j < roomWarnings.length; ++j) {
        this.infoBox_.pushWarningMessage(roomWarnings[j]);
      }
    }
  }
  this.call_.onremotehangup = this.onRemoteHangup_.bind(this);
  this.call_.onremotesdpset = this.onRemoteSdpSet_.bind(this);
  this.call_.onremotestreamadded = this.onRemoteStreamAdded_.bind(this);
  this.call_.onlocalstreamadded = this.onLocalStreamAdded_.bind(this);
  this.call_.onsignalingstatechange = this.infoBox_.updateInfoDiv.bind(this.infoBox_);
  this.call_.oniceconnectionstatechange = this.infoBox_.updateInfoDiv.bind(this.infoBox_);
  this.call_.onnewicecandidate = this.infoBox_.recordIceCandidateTypes.bind(this.infoBox_);
  this.call_.onerror = this.displayError_.bind(this);
  this.call_.onstatusmessage = this.displayStatus_.bind(this);
  this.call_.oncallerstarted = this.displaySharingInfo_.bind(this);
};
AppController.prototype.showRoomSelection_ = function() {
  var roomSelectionDiv = $(UI_CONSTANTS.roomSelectionDiv);
  this.roomSelection_ = new RoomSelection(roomSelectionDiv, UI_CONSTANTS);
  this.show_(roomSelectionDiv);
  this.roomSelection_.onRoomSelected = function(roomName) {
    this.hide_(roomSelectionDiv);
    this.createCall_();
    this.finishCallSetup_(roomName);
    this.roomSelection_.removeEventListeners();
    this.roomSelection_ = null;
    if (this.localStream_) {
      this.attachLocalStream_();
    }
  }.bind(this);
};
AppController.prototype.setupUi_ = function() {
  this.iconEventSetup_();
  document.onkeypress = this.onKeyPress_.bind(this);
  window.onmousemove = this.showIcons_.bind(this);
  $(UI_CONSTANTS.muteAudioSvg).onclick = this.toggleAudioMute_.bind(this);
  $(UI_CONSTANTS.muteVideoSvg).onclick = this.toggleVideoMute_.bind(this);
  $(UI_CONSTANTS.fullscreenSvg).onclick = this.toggleFullScreen_.bind(this);
  $(UI_CONSTANTS.hangupSvg).onclick = this.hangup_.bind(this);
  setUpFullScreen();
};
AppController.prototype.finishCallSetup_ = function(roomId) {
  this.call_.start(roomId);
  this.setupUi_();
  if (!isChromeApp()) {
    window.onbeforeunload = function() {
      this.call_.hangup(false);
    }.bind(this);
    window.onpopstate = function(event) {
      if (!event.state) {
        trace("Reloading main page.");
        location.href = location.origin;
      } else {
        if (event.state.roomLink) {
          location.href = event.state.roomLink;
        }
      }
    };
  }
};
AppController.prototype.hangup_ = function() {
  trace("Hanging up.");
  this.hide_(this.icons_);
  this.displayStatus_("Hanging up");
  this.transitionToDone_();
  this.call_.hangup(true);
  document.onkeypress = null;
  window.onmousemove = null;
};
AppController.prototype.onRemoteHangup_ = function() {
  this.displayStatus_("The remote side hung up.");
  this.transitionToWaiting_();
  this.call_.onRemoteHangup();
};
AppController.prototype.onRemoteSdpSet_ = function(hasRemoteVideo) {
  if (hasRemoteVideo) {
    trace("Waiting for remote video.");
    this.waitForRemoteVideo_();
  } else {
    trace("No remote video stream; not waiting for media to arrive.");
    this.transitionToActive_();
  }
};
AppController.prototype.waitForRemoteVideo_ = function() {
  if (this.remoteVideo_.readyState >= 2) {
    trace("Remote video started; currentTime: " + this.remoteVideo_.currentTime);
    this.transitionToActive_();
  } else {
    this.remoteVideo_.oncanplay = this.waitForRemoteVideo_.bind(this);
  }
};
AppController.prototype.onRemoteStreamAdded_ = function(stream) {
  this.deactivate_(this.sharingDiv_);
  trace("Remote stream added.");
  this.remoteVideo_.srcObject = stream;
  this.infoBox_.getRemoteTrackIds(stream);
  if (this.remoteVideoResetTimer_) {
    clearTimeout(this.remoteVideoResetTimer_);
    this.remoteVideoResetTimer_ = null;
  }
};
AppController.prototype.onLocalStreamAdded_ = function(stream) {
  trace("User has granted access to local media.");
  this.localStream_ = stream;
  this.infoBox_.getLocalTrackIds(this.localStream_);
  if (!this.roomSelection_) {
    this.attachLocalStream_();
  }
};
AppController.prototype.attachLocalStream_ = function() {
  trace("Attaching local stream.");
  this.localVideo_.srcObject = this.localStream_;
  this.displayStatus_("");
  this.activate_(this.localVideo_);
  this.show_(this.icons_);
  if (this.localStream_.getVideoTracks().length === 0) {
    this.hide_($(UI_CONSTANTS.muteVideoSvg));
  }
  if (this.localStream_.getAudioTracks().length === 0) {
    this.hide_($(UI_CONSTANTS.muteAudioSvg));
  }
};
AppController.prototype.transitionToActive_ = function() {
  this.remoteVideo_.oncanplay = undefined;
  var connectTime = window.performance.now();
  this.infoBox_.setSetupTimes(this.call_.startTime, connectTime);
  this.infoBox_.updateInfoDiv();
  trace("Call setup time: " + (connectTime - this.call_.startTime).toFixed(0) + "ms.");
  trace("reattachMediaStream: " + this.localVideo_.srcObject);
  this.miniVideo_.srcObject = this.localVideo_.srcObject;
  this.activate_(this.remoteVideo_);
  this.activate_(this.miniVideo_);
  this.deactivate_(this.localVideo_);
  this.localVideo_.srcObject = null;
  this.activate_(this.videosDiv_);
  this.show_(this.hangupSvg_);
  this.displayStatus_("");
};
AppController.prototype.transitionToWaiting_ = function() {
  this.remoteVideo_.oncanplay = undefined;
  this.hide_(this.hangupSvg_);
  this.deactivate_(this.videosDiv_);
  if (!this.remoteVideoResetTimer_) {
    this.remoteVideoResetTimer_ = setTimeout(function() {
      this.remoteVideoResetTimer_ = null;
      trace("Resetting remoteVideo src after transitioning to waiting.");
      this.remoteVideo_.srcObject = null;
    }.bind(this), 800);
  }
  this.localVideo_.srcObject = this.miniVideo_.srcObject;
  this.activate_(this.localVideo_);
  this.deactivate_(this.remoteVideo_);
  this.deactivate_(this.miniVideo_);
};
AppController.prototype.transitionToDone_ = function() {
  this.remoteVideo_.oncanplay = undefined;
  this.deactivate_(this.localVideo_);
  this.deactivate_(this.remoteVideo_);
  this.deactivate_(this.miniVideo_);
  this.hide_(this.hangupSvg_);
  this.activate_(this.rejoinDiv_);
  this.show_(this.rejoinDiv_);
  this.displayStatus_("");
};
AppController.prototype.onRejoinClick_ = function() {
  this.deactivate_(this.rejoinDiv_);
  this.hide_(this.rejoinDiv_);
  this.call_.restart();
  this.setupUi_();
};
AppController.prototype.onNewRoomClick_ = function() {
  this.deactivate_(this.rejoinDiv_);
  this.hide_(this.rejoinDiv_);
  this.showRoomSelection_();
};
AppController.prototype.onKeyPress_ = function(event) {
  switch(String.fromCharCode(event.charCode)) {
    case " ":
    case "m":
      if (this.call_) {
        this.call_.toggleAudioMute();
        this.muteAudioIconSet_.toggle();
      }
      return false;
    case "c":
      if (this.call_) {
        this.call_.toggleVideoMute();
        this.muteVideoIconSet_.toggle();
      }
      return false;
    case "f":
      this.toggleFullScreen_();
      return false;
    case "i":
      this.infoBox_.toggleInfoDiv();
      return false;
    case "q":
      this.hangup_();
      return false;
    case "l":
      this.toggleMiniVideo_();
      return false;
    default:
      return;
  }
};
AppController.prototype.pushCallNavigation_ = function(roomId, roomLink) {
  if (!isChromeApp()) {
    window.history.pushState({"roomId":roomId, "roomLink":roomLink}, roomId, roomLink);
  }
};
AppController.prototype.displaySharingInfo_ = function(roomId, roomLink) {
  this.roomLinkHref_.href = roomLink;
  this.roomLinkHref_.text = roomLink;
  this.roomLink_ = roomLink;
  this.pushCallNavigation_(roomId, roomLink);
  this.activate_(this.sharingDiv_);
};
AppController.prototype.displayStatus_ = function(status) {
  if (status === "") {
    this.deactivate_(this.statusDiv_);
  } else {
    this.activate_(this.statusDiv_);
  }
  this.statusDiv_.innerHTML = status;
};
AppController.prototype.displayError_ = function(error) {
  trace(error);
  this.infoBox_.pushErrorMessage(error);
};
AppController.prototype.toggleAudioMute_ = function() {
  this.call_.toggleAudioMute();
  this.muteAudioIconSet_.toggle();
};
AppController.prototype.toggleVideoMute_ = function() {
  this.call_.toggleVideoMute();
  this.muteVideoIconSet_.toggle();
};
AppController.prototype.toggleFullScreen_ = function() {
  if (isFullScreen()) {
    trace("Exiting fullscreen.");
    document.querySelector("svg#fullscreen title").textContent = "Enter fullscreen";
    document.cancelFullScreen();
  } else {
    trace("Entering fullscreen.");
    document.querySelector("svg#fullscreen title").textContent = "Exit fullscreen";
    document.body.requestFullScreen();
  }
  this.fullscreenIconSet_.toggle();
};
AppController.prototype.toggleMiniVideo_ = function() {
  if (this.miniVideo_.classList.contains("active")) {
    this.deactivate_(this.miniVideo_);
  } else {
    this.activate_(this.miniVideo_);
  }
};
AppController.prototype.hide_ = function(element) {
  element.classList.add("hidden");
};
AppController.prototype.show_ = function(element) {
  element.classList.remove("hidden");
};
AppController.prototype.activate_ = function(element) {
  element.classList.add("active");
};
AppController.prototype.deactivate_ = function(element) {
  element.classList.remove("active");
};
AppController.prototype.showIcons_ = function() {
  if (!this.icons_.classList.contains("active")) {
    this.activate_(this.icons_);
    this.setIconTimeout_();
  }
};
AppController.prototype.hideIcons_ = function() {
  if (this.icons_.classList.contains("active")) {
    this.deactivate_(this.icons_);
  }
};
AppController.prototype.setIconTimeout_ = function() {
  if (this.hideIconsAfterTimeout) {
    window.clearTimeout.bind(this, this.hideIconsAfterTimeout);
  }
  this.hideIconsAfterTimeout = window.setTimeout(function() {
    this.hideIcons_();
  }.bind(this), 5000);
};
AppController.prototype.iconEventSetup_ = function() {
  this.icons_.onmouseenter = function() {
    window.clearTimeout(this.hideIconsAfterTimeout);
  }.bind(this);
  this.icons_.onmouseleave = function() {
    this.setIconTimeout_();
  }.bind(this);
};
AppController.prototype.loadUrlParams_ = function() {
  var DEFAULT_VIDEO_CODEC = "VP9";
  var urlParams = queryStringToDictionary(window.location.search);
  this.loadingParams_.audioSendBitrate = urlParams["asbr"];
  this.loadingParams_.audioSendCodec = urlParams["asc"];
  this.loadingParams_.audioRecvBitrate = urlParams["arbr"];
  this.loadingParams_.audioRecvCodec = urlParams["arc"];
  this.loadingParams_.opusMaxPbr = urlParams["opusmaxpbr"];
  this.loadingParams_.opusFec = urlParams["opusfec"];
  this.loadingParams_.opusDtx = urlParams["opusdtx"];
  this.loadingParams_.opusStereo = urlParams["stereo"];
  this.loadingParams_.videoSendBitrate = urlParams["vsbr"];
  this.loadingParams_.videoSendInitialBitrate = urlParams["vsibr"];
  this.loadingParams_.videoSendCodec = urlParams["vsc"];
  this.loadingParams_.videoRecvBitrate = urlParams["vrbr"];
  this.loadingParams_.videoRecvCodec = urlParams["vrc"] || DEFAULT_VIDEO_CODEC;
  this.loadingParams_.videoFec = urlParams["videofec"];
};
AppController.IconSet_ = function(iconSelector) {
  this.iconElement = document.querySelector(iconSelector);
};
AppController.IconSet_.prototype.toggle = function() {
  if (this.iconElement.classList.contains("on")) {
    this.iconElement.classList.remove("on");
  } else {
    this.iconElement.classList.add("on");
  }
};
var Call = function(params) {
  this.params_ = params;
  this.roomServer_ = params.roomServer || "";
  this.channel_ = new SignalingChannel(params.wssUrl, params.wssPostUrl);
  this.channel_.onmessage = this.onRecvSignalingChannelMessage_.bind(this);
  this.pcClient_ = null;
  this.localStream_ = null;
  this.errorMessageQueue_ = [];
  this.startTime = null;
  this.oncallerstarted = null;
  this.onerror = null;
  this.oniceconnectionstatechange = null;
  this.onlocalstreamadded = null;
  this.onnewicecandidate = null;
  this.onremotehangup = null;
  this.onremotesdpset = null;
  this.onremotestreamadded = null;
  this.onsignalingstatechange = null;
  this.onstatusmessage = null;
  this.getMediaPromise_ = null;
  this.getIceServersPromise_ = null;
  this.requestMediaAndIceServers_();
};
Call.prototype.requestMediaAndIceServers_ = function() {
  this.getMediaPromise_ = this.maybeGetMedia_();
  this.getIceServersPromise_ = this.maybeGetIceServers_();
};
Call.prototype.isInitiator = function() {
  return this.params_.isInitiator;
};
Call.prototype.start = function(roomId) {
  this.connectToRoom_(roomId);
  if (this.params_.isLoopback) {
    setupLoopback(this.params_.wssUrl, roomId);
  }
};
Call.prototype.queueCleanupMessages_ = function() {
  apprtc.windowPort.sendMessage({action:Constants.QUEUEADD_ACTION, queueMessage:{action:Constants.XHR_ACTION, method:"POST", url:this.getLeaveUrl_(), body:null}});
  apprtc.windowPort.sendMessage({action:Constants.QUEUEADD_ACTION, queueMessage:{action:Constants.WS_ACTION, wsAction:Constants.WS_SEND_ACTION, data:JSON.stringify({cmd:"send", msg:JSON.stringify({type:"bye"})})}});
  apprtc.windowPort.sendMessage({action:Constants.QUEUEADD_ACTION, queueMessage:{action:Constants.XHR_ACTION, method:"DELETE", url:this.channel_.getWssPostUrl(), body:null}});
};
Call.prototype.clearCleanupQueue_ = function() {
  apprtc.windowPort.sendMessage({action:Constants.QUEUECLEAR_ACTION});
};
Call.prototype.restart = function() {
  this.requestMediaAndIceServers_();
  this.start(this.params_.previousRoomId);
};
Call.prototype.hangup = function(async) {
  this.startTime = null;
  if (isChromeApp()) {
    this.clearCleanupQueue_();
  }
  if (this.localStream_) {
    if (typeof this.localStream_.getTracks === "undefined") {
      this.localStream_.stop();
    } else {
      this.localStream_.getTracks().forEach(function(track) {
        track.stop();
      });
    }
    this.localStream_ = null;
  }
  if (!this.params_.roomId) {
    return;
  }
  if (this.pcClient_) {
    this.pcClient_.close();
    this.pcClient_ = null;
  }
  var steps = [];
  steps.push({step:function() {
    var path = this.getLeaveUrl_();
    return sendUrlRequest("POST", path, async);
  }.bind(this), errorString:"Error sending /leave:"});
  steps.push({step:function() {
    this.channel_.send(JSON.stringify({type:"bye"}));
  }.bind(this), errorString:"Error sending bye:"});
  steps.push({step:function() {
    return this.channel_.close(async);
  }.bind(this), errorString:"Error closing signaling channel:"});
  steps.push({step:function() {
    this.params_.previousRoomId = this.params_.roomId;
    this.params_.roomId = null;
    this.params_.clientId = null;
  }.bind(this), errorString:"Error setting params:"});
  if (async) {
    var errorHandler = function(errorString, error) {
      trace(errorString + " " + error.message);
    };
    var promise = Promise.resolve();
    for (var i = 0; i < steps.length; ++i) {
      promise = promise.then(steps[i].step).catch(errorHandler.bind(this, steps[i].errorString));
    }
    return promise;
  }
  var executeStep = function(executor, errorString) {
    try {
      executor();
    } catch (ex) {
      trace(errorString + " " + ex);
    }
  };
  for (var j = 0; j < steps.length; ++j) {
    executeStep(steps[j].step, steps[j].errorString);
  }
  if (this.params_.roomId !== null || this.params_.clientId !== null) {
    trace("ERROR: sync cleanup tasks did not complete successfully.");
  } else {
    trace("Cleanup completed.");
  }
  return Promise.resolve();
};
Call.prototype.getLeaveUrl_ = function() {
  return this.roomServer_ + "/leave/" + this.params_.roomId + "/" + this.params_.clientId;
};
Call.prototype.onRemoteHangup = function() {
  this.startTime = null;
  this.params_.isInitiator = true;
  if (this.pcClient_) {
    this.pcClient_.close();
    this.pcClient_ = null;
  }
  this.startSignaling_();
};
Call.prototype.getPeerConnectionStates = function() {
  if (!this.pcClient_) {
    return null;
  }
  return this.pcClient_.getPeerConnectionStates();
};
Call.prototype.getPeerConnectionStats = function(callback) {
  if (!this.pcClient_) {
    return;
  }
  this.pcClient_.getPeerConnectionStats(callback);
};
Call.prototype.toggleVideoMute = function() {
  var videoTracks = this.localStream_.getVideoTracks();
  if (videoTracks.length === 0) {
    trace("No local video available.");
    return;
  }
  trace("Toggling video mute state.");
  for (var i = 0; i < videoTracks.length; ++i) {
    videoTracks[i].enabled = !videoTracks[i].enabled;
  }
  trace("Video " + (videoTracks[0].enabled ? "unmuted." : "muted."));
};
Call.prototype.toggleAudioMute = function() {
  var audioTracks = this.localStream_.getAudioTracks();
  if (audioTracks.length === 0) {
    trace("No local audio available.");
    return;
  }
  trace("Toggling audio mute state.");
  for (var i = 0; i < audioTracks.length; ++i) {
    audioTracks[i].enabled = !audioTracks[i].enabled;
  }
  trace("Audio " + (audioTracks[0].enabled ? "unmuted." : "muted."));
};
Call.prototype.connectToRoom_ = function(roomId) {
  this.params_.roomId = roomId;
  var channelPromise = this.channel_.open().catch(function(error) {
    this.onError_("WebSocket open error: " + error.message);
    return Promise.reject(error);
  }.bind(this));
  var joinPromise = this.joinRoom_().then(function(roomParams) {
    this.params_.clientId = roomParams.client_id;
    this.params_.roomId = roomParams.room_id;
    this.params_.roomLink = roomParams.room_link;
    this.params_.isInitiator = roomParams.is_initiator === "true";
    this.params_.messages = roomParams.messages;
  }.bind(this)).catch(function(error) {
    this.onError_("Room server join error: " + error.message);
    return Promise.reject(error);
  }.bind(this));
  Promise.all([channelPromise, joinPromise]).then(function() {
    this.channel_.register(this.params_.roomId, this.params_.clientId);
    Promise.all([this.getIceServersPromise_, this.getMediaPromise_]).then(function() {
      this.startSignaling_();
      if (isChromeApp()) {
        this.queueCleanupMessages_();
      }
    }.bind(this)).catch(function(error) {
      this.onError_("Failed to start signaling: " + error.message);
    }.bind(this));
  }.bind(this)).catch(function(error) {
    this.onError_("WebSocket register error: " + error.message);
  }.bind(this));
};
Call.prototype.maybeGetMedia_ = function() {
  var needStream = this.params_.mediaConstraints.audio !== false || this.params_.mediaConstraints.video !== false;
  var mediaPromise = null;
  if (needStream) {
    var mediaConstraints = this.params_.mediaConstraints;
    mediaPromise = navigator.mediaDevices.getUserMedia(mediaConstraints).catch(function(error) {
      if (error.name !== "NotFoundError") {
        throw error;
      }
      return navigator.mediaDevices.enumerateDevices().then(function(devices) {
        var cam = devices.find(function(device) {
          return device.kind === "videoinput";
        });
        var mic = devices.find(function(device) {
          return device.kind === "audioinput";
        });
        var constraints = {video:cam && mediaConstraints.video, audio:mic && mediaConstraints.audio};
        return navigator.mediaDevices.getUserMedia(constraints);
      });
    }).then(function(stream) {
      trace("Got access to local media with mediaConstraints:\n" + "  '" + JSON.stringify(mediaConstraints) + "'");
      this.onUserMediaSuccess_(stream);
    }.bind(this)).catch(function(error) {
      this.onError_("Error getting user media: " + error.message);
      this.onUserMediaError_(error);
    }.bind(this));
  } else {
    mediaPromise = Promise.resolve();
  }
  return mediaPromise;
};
Call.prototype.maybeGetIceServers_ = function() {
  var shouldRequestIceServers = this.params_.iceServerRequestUrl && this.params_.iceServerRequestUrl.length > 0 && this.params_.peerConnectionConfig.iceServers && this.params_.peerConnectionConfig.iceServers.length === 0;
  var iceServerPromise = null;
  if (shouldRequestIceServers) {
    var requestUrl = this.params_.iceServerRequestUrl;
    iceServerPromise = requestIceServers(requestUrl, this.params_.iceServerTransports).then(function(iceServers) {
      var servers = this.params_.peerConnectionConfig.iceServers;
      this.params_.peerConnectionConfig.iceServers = servers.concat(iceServers);
    }.bind(this)).catch(function(error) {
      if (this.onstatusmessage) {
        var subject = encodeURIComponent("AppRTC demo ICE servers not working");
        this.onstatusmessage("No TURN server; unlikely that media will traverse networks. " + "If this persists please " + '<a href="mailto:discuss-webrtc@googlegroups.com?' + "subject=" + subject + '">' + "report it to discuss-webrtc@googlegroups.com</a>.");
      }
      trace(error.message);
    }.bind(this));
  } else {
    iceServerPromise = Promise.resolve();
  }
  return iceServerPromise;
};
Call.prototype.onUserMediaSuccess_ = function(stream) {
  this.localStream_ = stream;
  if (this.onlocalstreamadded) {
    this.onlocalstreamadded(stream);
  }
};
Call.prototype.onUserMediaError_ = function(error) {
  var errorMessage = "Failed to get access to local media. Error name was " + error.name + ". Continuing without sending a stream.";
  this.onError_("getUserMedia error: " + errorMessage);
  this.errorMessageQueue_.push(error);
  alert(errorMessage);
};
Call.prototype.maybeCreatePcClientAsync_ = function() {
  return new Promise(function(resolve, reject) {
    if (this.pcClient_) {
      resolve();
      return;
    }
    if (typeof RTCPeerConnection.generateCertificate === "function") {
      var certParams = {name:"ECDSA", namedCurve:"P-256"};
      RTCPeerConnection.generateCertificate(certParams).then(function(cert) {
        trace("ECDSA certificate generated successfully.");
        this.params_.peerConnectionConfig.certificates = [cert];
        this.createPcClient_();
        resolve();
      }.bind(this)).catch(function(error) {
        trace("ECDSA certificate generation failed.");
        reject(error);
      });
    } else {
      this.createPcClient_();
      resolve();
    }
  }.bind(this));
};
Call.prototype.createPcClient_ = function() {
  this.pcClient_ = new PeerConnectionClient(this.params_, this.startTime);
  this.pcClient_.onsignalingmessage = this.sendSignalingMessage_.bind(this);
  this.pcClient_.onremotehangup = this.onremotehangup;
  this.pcClient_.onremotesdpset = this.onremotesdpset;
  this.pcClient_.onremotestreamadded = this.onremotestreamadded;
  this.pcClient_.onsignalingstatechange = this.onsignalingstatechange;
  this.pcClient_.oniceconnectionstatechange = this.oniceconnectionstatechange;
  this.pcClient_.onnewicecandidate = this.onnewicecandidate;
  this.pcClient_.onerror = this.onerror;
  trace("Created PeerConnectionClient");
};
Call.prototype.startSignaling_ = function() {
  trace("Starting signaling.");
  if (this.isInitiator() && this.oncallerstarted) {
    this.oncallerstarted(this.params_.roomId, this.params_.roomLink);
  }
  this.startTime = window.performance.now();
  this.maybeCreatePcClientAsync_().then(function() {
    if (this.localStream_) {
      trace("Adding local stream.");
      this.pcClient_.addStream(this.localStream_);
    }
    if (this.params_.isInitiator) {
      this.pcClient_.startAsCaller(this.params_.offerOptions);
    } else {
      this.pcClient_.startAsCallee(this.params_.messages);
    }
  }.bind(this)).catch(function(e) {
    this.onError_("Create PeerConnection exception: " + e);
    alert("Cannot create RTCPeerConnection: " + e.message);
  }.bind(this));
};
Call.prototype.joinRoom_ = function() {
  return new Promise(function(resolve, reject) {
    if (!this.params_.roomId) {
      reject(Error("Missing room id."));
    }
    var path = this.roomServer_ + "/join/" + this.params_.roomId + window.location.search;
    sendAsyncUrlRequest("POST", path).then(function(response) {
      var responseObj = parseJSON(response);
      if (!responseObj) {
        reject(Error("Error parsing response JSON."));
        return;
      }
      if (responseObj.result !== "SUCCESS") {
        reject(Error("Registration error: " + responseObj.result));
        if (responseObj.result === "FULL") {
          var getPath = this.roomServer_ + "/r/" + this.params_.roomId + window.location.search;
          window.location.assign(getPath);
        }
        return;
      }
      trace("Joined the room.");
      resolve(responseObj.params);
    }.bind(this)).catch(function(error) {
      reject(Error("Failed to join the room: " + error.message));
      return;
    }.bind(this));
  }.bind(this));
};
Call.prototype.onRecvSignalingChannelMessage_ = function(msg) {
  this.maybeCreatePcClientAsync_().then(this.pcClient_.receiveSignalingMessage(msg));
};
Call.prototype.sendSignalingMessage_ = function(message) {
  var msgString = JSON.stringify(message);
  if (this.params_.isInitiator) {
    var path = this.roomServer_ + "/sgc/protocol/apprtc.esegece.com/message/" + this.params_.roomId + "/" + this.params_.clientId + window.location.search;
    var xhr = new XMLHttpRequest;
    xhr.open("POST", path, true);
    xhr.send(msgString);
    trace("C->GAE: " + msgString);
  } else {
    this.channel_.send(msgString);
  }
};
Call.prototype.onError_ = function(message) {
  if (this.onerror) {
    this.onerror(message);
  }
};
var Constants = {WS_ACTION:"ws", XHR_ACTION:"xhr", QUEUEADD_ACTION:"addToQueue", QUEUECLEAR_ACTION:"clearQueue", EVENT_ACTION:"event", WS_CREATE_ACTION:"create", WS_EVENT_ONERROR:"onerror", WS_EVENT_ONMESSAGE:"onmessage", WS_EVENT_ONOPEN:"onopen", WS_EVENT_ONCLOSE:"onclose", WS_EVENT_SENDERROR:"onsenderror", WS_SEND_ACTION:"send", WS_CLOSE_ACTION:"close"};
var InfoBox = function(infoDiv, call, versionInfo) {
  this.infoDiv_ = infoDiv;
  this.remoteVideo_ = document.getElementById("remote-video");
  this.localVideo_ = document.getElementById("mini-video");
  this.call_ = call;
  this.versionInfo_ = versionInfo;
  this.errorMessages_ = [];
  this.warningMessages_ = [];
  this.startTime_ = null;
  this.connectTime_ = null;
  this.stats_ = null;
  this.prevStats_ = null;
  this.getStatsTimer_ = null;
  this.localTrackIds_ = {video:"", audio:""};
  this.remoteTrackIds_ = {video:"", audio:""};
  this.iceCandidateTypes_ = {Local:{}, Remote:{}};
  this.localDecodedFrames_ = 0;
  this.localStartTime_ = 0;
  this.localVideo_.addEventListener("playing", function(event) {
    this.localDecodedFrames_ = event.target.webkitDecodedFrameCount;
    this.localStartTime_ = (new Date).getTime();
  }.bind(this));
  this.remoteDecodedFrames_ = 0;
  this.remoteStartTime_ = 0;
  this.remoteVideo_.addEventListener("playing", function(event) {
    this.remoteDecodedFrames_ = event.target.webkitDecodedFrameCount;
    this.remoteStartTime_ = (new Date).getTime();
  }.bind(this));
};
InfoBox.prototype.getLocalTrackIds = function(stream) {
  stream.getTracks().forEach(function(track) {
    if (track.kind === "audio") {
      this.localTrackIds_.audio = track.id;
    } else {
      if (track.kind === "video") {
        this.localTrackIds_.video = track.id;
      }
    }
  }.bind(this));
};
InfoBox.prototype.getRemoteTrackIds = function(stream) {
  stream.getTracks().forEach(function(track) {
    if (track.kind === "audio") {
      this.remoteTrackIds_.audio = track.id;
    } else {
      if (track.kind === "video") {
        this.remoteTrackIds_.video = track.id;
      }
    }
  }.bind(this));
};
InfoBox.prototype.recordIceCandidateTypes = function(location, candidate) {
  var type = iceCandidateType(candidate);
  var types = this.iceCandidateTypes_[location];
  if (!types[type]) {
    types[type] = 1;
  } else {
    ++types[type];
  }
  this.updateInfoDiv();
};
InfoBox.prototype.pushErrorMessage = function(msg) {
  this.errorMessages_.push(msg);
  this.updateInfoDiv();
  this.showInfoDiv();
};
InfoBox.prototype.pushWarningMessage = function(msg) {
  this.warningMessages_.push(msg);
  this.updateInfoDiv();
  this.showInfoDiv();
};
InfoBox.prototype.setSetupTimes = function(startTime, connectTime) {
  this.startTime_ = startTime;
  this.connectTime_ = connectTime;
};
InfoBox.prototype.showInfoDiv = function() {
  this.getStatsTimer_ = setInterval(this.refreshStats_.bind(this), 1000);
  this.refreshStats_();
  this.infoDiv_.classList.add("active");
};
InfoBox.prototype.toggleInfoDiv = function() {
  if (this.infoDiv_.classList.contains("active")) {
    clearInterval(this.getStatsTimer_);
    this.infoDiv_.classList.remove("active");
  } else {
    this.showInfoDiv();
  }
};
InfoBox.prototype.refreshStats_ = function() {
  this.call_.getPeerConnectionStats(function(response) {
    this.prevStats_ = this.stats_;
    this.stats_ = response;
    this.updateInfoDiv();
  }.bind(this));
};
InfoBox.prototype.updateInfoDiv = function() {
  var contents = '<pre id="info-box-stats" style="line-height: initial">';
  if (this.stats_) {
    var states = this.call_.getPeerConnectionStates();
    if (!states) {
      return;
    }
    contents += this.buildLine_("States");
    contents += this.buildLine_("Signaling", states.signalingState);
    contents += this.buildLine_("Gathering", states.iceGatheringState);
    contents += this.buildLine_("Connection", states.iceConnectionState);
    for (var endpoint in this.iceCandidateTypes_) {
      var types = [];
      for (var type in this.iceCandidateTypes_[endpoint]) {
        types.push(type + ":" + this.iceCandidateTypes_[endpoint][type]);
      }
      contents += this.buildLine_(endpoint, types.join(" "));
    }
    var statReport = enumerateStats(this.stats_, this.localTrackIds_, this.remoteTrackIds_);
    var connectionStats = statReport.connection;
    var localAddr;
    var remoteAddr;
    var localAddrType;
    var remoteAddrType;
    var localPort;
    var remotePort;
    if (connectionStats) {
      localAddr = connectionStats.localIp;
      remoteAddr = connectionStats.remoteIp;
      localAddrType = connectionStats.localType;
      remoteAddrType = connectionStats.remoteType;
      localPort = connectionStats.localPort;
      remotePort = connectionStats.remotePort;
    }
    if (localAddr && remoteAddr) {
      var localCandId = connectionStats.localCandidateId;
      var localTypePref;
      if (localCandId) {
        localTypePref = connectionStats.localPriority >> 24;
      }
      contents += this.buildLine_("LocalAddr", localAddr + " (" + localAddrType + (typeof localTypePref !== undefined ? "" + formatTypePreference(localTypePref) : "") + ")");
      contents += this.buildLine_("LocalPort", localPort);
      contents += this.buildLine_("RemoteAddr", remoteAddr + " (" + remoteAddrType + ")");
      contents += this.buildLine_("RemotePort", remotePort);
    }
    contents += this.buildLine_();
    contents += this.buildStatsSection_();
  }
  if (this.errorMessages_.length > 0 || this.warningMessages_.length > 0) {
    contents += this.buildLine_("\nMessages");
    if (this.errorMessages_.length) {
      this.infoDiv_.classList.add("warning");
      for (var i = 0; i !== this.errorMessages_.length; ++i) {
        contents += this.errorMessages_[i] + "\n";
      }
    } else {
      this.infoDiv_.classList.add("active");
      for (var j = 0; j !== this.warningMessages_.length; ++j) {
        contents += this.warningMessages_[j] + "\n";
      }
    }
  } else {
    this.infoDiv_.classList.remove("warning");
  }
  if (this.versionInfo_) {
    contents += this.buildLine_();
    contents += this.buildLine_("Version");
    for (var key in this.versionInfo_) {
      contents += this.buildLine_(key, this.versionInfo_[key]);
    }
  }
  contents += "</pre>";
  if (this.infoDiv_.innerHTML !== contents) {
    this.infoDiv_.innerHTML = contents;
  }
};
InfoBox.prototype.buildStatsSection_ = function() {
  var contents = this.buildLine_("Stats");
  var statReport = enumerateStats(this.stats_, this.localTrackIds_, this.remoteTrackIds_);
  var prevStatReport = enumerateStats(this.prevStats_, this.localTrackIds_, this.remoteTrackIds_);
  var totalRtt = statReport.connection.totalRoundTripTime * 1000;
  var currentRtt = statReport.connection.currentRoundTripTime * 1000;
  if (this.endTime_ !== null) {
    contents += this.buildLine_("Call time", InfoBox.formatInterval_(window.performance.now() - this.connectTime_));
    contents += this.buildLine_("Setup time", InfoBox.formatMsec_(this.connectTime_ - this.startTime_));
  }
  if (statReport.connection.remoteIp !== "") {
    contents += this.buildLine_("TotalRtt", InfoBox.formatMsec_(totalRtt));
    contents += this.buildLine_("CurrentRtt", InfoBox.formatMsec_(currentRtt));
  }
  var rxAudio = statReport.audio.remote;
  var rxPrevAudio = prevStatReport.audio.remote;
  var rxPrevVideo = prevStatReport.video.remote;
  var rxVideo = statReport.video.remote;
  var txAudio = statReport.audio.local;
  var txPrevAudio = prevStatReport.audio.local;
  var txPrevVideo = prevStatReport.video.local;
  var txVideo = statReport.video.local;
  var rxAudioBitrate;
  var rxAudioClockRate;
  var rxAudioCodec;
  var rxAudioJitter;
  var rxAudioLevel;
  var rxAudioPacketRate;
  var rxAudioPlType;
  var rxVideoBitrate;
  var rxVideoCodec;
  var rxVideoDroppedFrames;
  var rxVideoFirCount;
  var rxVideoFps;
  var rxVideoHeight;
  var rxVideoNackCount;
  var rxVideoPacketRate;
  var rxVideoPliCount;
  var rxVideoPlType;
  var txAudioBitrate;
  var txAudioClockRate;
  var txAudioCodec;
  var txAudioLevel;
  var txAudioPacketRate;
  var txAudioPlType;
  var txVideoBitrate;
  var txVideoCodec;
  var txVideoFirCount;
  var txVideoFps;
  var txVideoHeight;
  var txVideoNackCount;
  var txVideoPacketRate;
  var txVideoPliCount;
  var txVideoPlType;
  if (txAudio.codecId !== "" && txAudio.payloadType !== 0) {
    txAudioCodec = txAudio.mimeType;
    txAudioLevel = parseFloat(txAudio.audioLevel).toFixed(3);
    txAudioClockRate = txAudio.clockRate;
    txAudioPlType = txAudio.payloadType;
    txAudioBitrate = computeBitrate(txAudio, txPrevAudio, "bytesSent");
    txAudioPacketRate = computeRate(txAudio, txPrevAudio, "packetsSent");
    contents += this.buildLine_("Audio Tx", txAudioCodec + "/" + txAudioPlType + ", " + "rate " + txAudioClockRate + ", " + InfoBox.formatBitrate_(txAudioBitrate) + ", " + InfoBox.formatPacketRate_(txAudioPacketRate) + ", inputLevel " + txAudioLevel);
  }
  if (rxAudio.codecId !== "" && rxAudio.payloadType !== 0) {
    rxAudioCodec = rxAudio.mimeType;
    rxAudioLevel = parseFloat(rxAudio.audioLevel).toFixed(3);
    rxAudioJitter = parseFloat(rxAudio.jitter).toFixed(3);
    rxAudioClockRate = rxAudio.clockRate;
    rxAudioPlType = rxAudio.payloadType;
    rxAudioBitrate = computeBitrate(rxAudio, rxPrevAudio, "bytesReceived");
    rxAudioPacketRate = computeRate(rxAudio, rxPrevAudio, "packetsReceived");
    contents += this.buildLine_("Audio Rx", rxAudioCodec + "/" + rxAudioPlType + ", " + "rate " + rxAudioClockRate + ", " + "jitter " + rxAudioJitter + ", " + InfoBox.formatBitrate_(rxAudioBitrate) + ", " + InfoBox.formatPacketRate_(rxAudioPacketRate) + ", outputLevel " + rxAudioLevel);
  }
  if (txVideo.codecId !== "" && txVideo.payloadType !== 0 && txVideo.frameHeight !== 0) {
    txVideoCodec = txVideo.mimeType;
    txVideoHeight = txVideo.frameHeight;
    txVideoPlType = txVideo.payloadType;
    txVideoPliCount = txVideo.pliCount;
    txVideoFirCount = txVideo.firCount;
    txVideoNackCount = txVideo.nackCount;
    txVideoFps = calculateFps(this.remoteVideo_, this.remoteDecodedFrames_, this.remoteStartTime_, "local", this.updateDecodedFramesCallback_);
    txVideoBitrate = computeBitrate(txVideo, txPrevVideo, "bytesSent");
    txVideoPacketRate = computeRate(txVideo, txPrevVideo, "packetsSent");
    contents += this.buildLine_("Video Tx", txVideoCodec + "/" + txVideoPlType + ", " + txVideoHeight.toString() + "p" + txVideoFps.toString() + ", " + "firCount " + txVideoFirCount + ", " + "pliCount " + txVideoPliCount + ", " + "nackCount " + txVideoNackCount + ", " + InfoBox.formatBitrate_(txVideoBitrate) + ", " + InfoBox.formatPacketRate_(txVideoPacketRate));
  }
  if (rxVideo.codecId !== "" && rxVideo.payloadType !== 0 && txVideo.frameHeight !== 0) {
    rxVideoCodec = rxVideo.mimeType;
    rxVideoHeight = rxVideo.frameHeight;
    rxVideoPlType = rxVideo.payloadType;
    rxVideoDroppedFrames = rxVideo.framesDropped;
    rxVideoPliCount = rxVideo.pliCount;
    rxVideoFirCount = rxVideo.firCount;
    rxVideoNackCount = rxVideo.nackCount;
    rxVideoFps = calculateFps(this.remoteVideo_, this.remoteDecodedFrames_, this.remoteStartTime_, "remote", this.updateDecodedFramesCallback_);
    rxVideoBitrate = computeBitrate(rxVideo, rxPrevVideo, "bytesReceived");
    rxVideoPacketRate = computeRate(rxVideo, rxPrevVideo, "packetsReceived");
    contents += this.buildLine_("Video Rx", rxVideoCodec + "/" + rxVideoPlType + ", " + rxVideoHeight.toString() + "p" + rxVideoFps.toString() + ", " + "firCount " + rxVideoFirCount + ", " + "pliCount " + rxVideoPliCount + ", " + "nackCount " + rxVideoNackCount + ", " + "droppedFrames " + rxVideoDroppedFrames + ", " + InfoBox.formatBitrate_(rxVideoBitrate) + ", " + InfoBox.formatPacketRate_(rxVideoPacketRate));
  }
  return contents;
};
InfoBox.prototype.updateDecodedFramesCallback_ = function(decodedFrames_, startTime_, remoteOrLocal) {
  if (remoteOrLocal === "local") {
    this.localDecodedFrames_ = decodedFrames_;
    this.localStartTime_ = startTime_;
  } else {
    if (remoteOrLocal === "remote") {
      this.remoteDecodedFrames_ = decodedFrames_;
      this.remoteStartTime_ = startTime_;
    }
  }
};
InfoBox.prototype.buildLine_ = function(label, value) {
  var columnWidth = 12;
  var line = "";
  if (label) {
    line += label + ":";
    while (line.length < columnWidth) {
      line += " ";
    }
    if (value) {
      line += value;
    }
  }
  line += "\n";
  return line;
};
InfoBox.formatInterval_ = function(value) {
  var result = "";
  var seconds = Math.floor(value / 1000);
  var minutes = Math.floor(seconds / 60);
  var hours = Math.floor(minutes / 60);
  var formatTwoDigit = function(twodigit) {
    return (twodigit < 10 ? "0" : "") + twodigit.toString();
  };
  if (hours > 0) {
    result += formatTwoDigit(hours) + ":";
  }
  result += formatTwoDigit(minutes - hours * 60) + ":";
  result += formatTwoDigit(seconds - minutes * 60);
  return result;
};
InfoBox.formatMsec_ = function(value) {
  return value.toFixed(0).toString() + " ms";
};
InfoBox.formatBitrate_ = function(value) {
  if (!value) {
    return "- bps";
  }
  var suffix;
  if (value < 1000) {
    suffix = "bps";
  } else {
    if (value < 1000000) {
      suffix = "kbps";
      value /= 1000;
    } else {
      suffix = "Mbps";
      value /= 1000000;
    }
  }
  var str = value.toPrecision(3) + " " + suffix;
  return str;
};
InfoBox.formatPacketRate_ = function(value) {
  if (!value) {
    return "- pps";
  }
  return value.toPrecision(3) + " " + "pps";
};
var PeerConnectionClient = function(params, startTime) {
  this.params_ = params;
  this.startTime_ = startTime;
  trace("Creating RTCPeerConnnection with:\n" + "  config: '" + JSON.stringify(params.peerConnectionConfig) + "';\n" + "  constraints: '" + JSON.stringify(params.peerConnectionConstraints) + "'.");
  this.pc_ = new RTCPeerConnection(params.peerConnectionConfig, params.peerConnectionConstraints);
  this.pc_.onicecandidate = this.onIceCandidate_.bind(this);
  this.pc_.ontrack = this.onRemoteStreamAdded_.bind(this);
  this.pc_.onremovestream = trace.bind(null, "Remote stream removed.");
  this.pc_.onsignalingstatechange = this.onSignalingStateChanged_.bind(this);
  this.pc_.oniceconnectionstatechange = this.onIceConnectionStateChanged_.bind(this);
  window.dispatchEvent(new CustomEvent("pccreated", {detail:{pc:this, time:new Date, userId:this.params_.roomId + (this.isInitiator_ ? "-0" : "-1"), sessionId:this.params_.roomId}}));
  this.hasRemoteSdp_ = false;
  this.messageQueue_ = [];
  this.isInitiator_ = false;
  this.started_ = false;
  this.onerror = null;
  this.oniceconnectionstatechange = null;
  this.onnewicecandidate = null;
  this.onremotehangup = null;
  this.onremotesdpset = null;
  this.onremotestreamadded = null;
  this.onsignalingmessage = null;
  this.onsignalingstatechange = null;
};
PeerConnectionClient.DEFAULT_SDP_OFFER_OPTIONS_ = {offerToReceiveAudio:1, offerToReceiveVideo:1, voiceActivityDetection:false};
PeerConnectionClient.prototype.addStream = function(stream) {
  if (!this.pc_) {
    return;
  }
  this.pc_.addStream(stream);
};
PeerConnectionClient.prototype.startAsCaller = function(offerOptions) {
  if (!this.pc_) {
    return false;
  }
  if (this.started_) {
    return false;
  }
  this.isInitiator_ = true;
  this.started_ = true;
  var constraints = mergeConstraints(PeerConnectionClient.DEFAULT_SDP_OFFER_OPTIONS_, offerOptions);
  trace("Sending offer to peer, with constraints: \n'" + JSON.stringify(constraints) + "'.");
  this.pc_.createOffer(constraints).then(this.setLocalSdpAndNotify_.bind(this)).catch(this.onError_.bind(this, "createOffer"));
  return true;
};
PeerConnectionClient.prototype.startAsCallee = function(initialMessages) {
  if (!this.pc_) {
    return false;
  }
  if (this.started_) {
    return false;
  }
  this.isInitiator_ = false;
  this.started_ = true;
  if (initialMessages && initialMessages.length > 0) {
    for (var i = 0, len = initialMessages.length; i < len; i++) {
      this.receiveSignalingMessage(initialMessages[i]);
    }
    return true;
  }
  if (this.messageQueue_.length > 0) {
    this.drainMessageQueue_();
  }
  return true;
};
PeerConnectionClient.prototype.receiveSignalingMessage = function(message) {
  var messageObj = parseJSON(message);
  if (!messageObj) {
    return;
  }
  if (this.isInitiator_ && messageObj.type === "answer" || !this.isInitiator_ && messageObj.type === "offer") {
    this.hasRemoteSdp_ = true;
    this.messageQueue_.unshift(messageObj);
  } else {
    if (messageObj.type === "candidate") {
      this.messageQueue_.push(messageObj);
    } else {
      if (messageObj.type === "bye") {
        if (this.onremotehangup) {
          this.onremotehangup();
        }
      }
    }
  }
  this.drainMessageQueue_();
};
PeerConnectionClient.prototype.close = function() {
  if (!this.pc_) {
    return;
  }
  this.pc_.close();
  window.dispatchEvent(new CustomEvent("pcclosed", {detail:{pc:this, time:new Date}}));
  this.pc_ = null;
};
PeerConnectionClient.prototype.getPeerConnectionStates = function() {
  if (!this.pc_) {
    return null;
  }
  return {"signalingState":this.pc_.signalingState, "iceGatheringState":this.pc_.iceGatheringState, "iceConnectionState":this.pc_.iceConnectionState};
};
PeerConnectionClient.prototype.getPeerConnectionStats = function(callback) {
  if (!this.pc_) {
    return;
  }
  this.pc_.getStats(null).then(callback);
};
PeerConnectionClient.prototype.doAnswer_ = function() {
  trace("Sending answer to peer.");
  this.pc_.createAnswer().then(this.setLocalSdpAndNotify_.bind(this)).catch(this.onError_.bind(this, "createAnswer"));
};
PeerConnectionClient.prototype.setLocalSdpAndNotify_ = function(sessionDescription) {
  sessionDescription.sdp = maybePreferAudioReceiveCodec(sessionDescription.sdp, this.params_);
  sessionDescription.sdp = maybePreferVideoReceiveCodec(sessionDescription.sdp, this.params_);
  sessionDescription.sdp = maybeSetAudioReceiveBitRate(sessionDescription.sdp, this.params_);
  sessionDescription.sdp = maybeSetVideoReceiveBitRate(sessionDescription.sdp, this.params_);
  sessionDescription.sdp = maybeRemoveVideoFec(sessionDescription.sdp, this.params_);
  this.pc_.setLocalDescription(sessionDescription).then(trace.bind(null, "Set session description success.")).catch(this.onError_.bind(this, "setLocalDescription"));
  if (this.onsignalingmessage) {
    this.onsignalingmessage({sdp:sessionDescription.sdp, type:sessionDescription.type});
  }
};
PeerConnectionClient.prototype.setRemoteSdp_ = function(message) {
  message.sdp = maybeSetOpusOptions(message.sdp, this.params_);
  message.sdp = maybePreferAudioSendCodec(message.sdp, this.params_);
  message.sdp = maybePreferVideoSendCodec(message.sdp, this.params_);
  message.sdp = maybeSetAudioSendBitRate(message.sdp, this.params_);
  message.sdp = maybeSetVideoSendBitRate(message.sdp, this.params_);
  message.sdp = maybeSetVideoSendInitialBitRate(message.sdp, this.params_);
  message.sdp = maybeRemoveVideoFec(message.sdp, this.params_);
  this.pc_.setRemoteDescription(new RTCSessionDescription(message)).then(this.onSetRemoteDescriptionSuccess_.bind(this)).catch(this.onError_.bind(this, "setRemoteDescription"));
};
PeerConnectionClient.prototype.onSetRemoteDescriptionSuccess_ = function() {
  trace("Set remote session description success.");
  var remoteStreams = this.pc_.getRemoteStreams();
  if (this.onremotesdpset) {
    this.onremotesdpset(remoteStreams.length > 0 && remoteStreams[0].getVideoTracks().length > 0);
  }
};
PeerConnectionClient.prototype.processSignalingMessage_ = function(message) {
  if (message.type === "offer" && !this.isInitiator_) {
    if (this.pc_.signalingState !== "stable") {
      trace("ERROR: remote offer received in unexpected state: " + this.pc_.signalingState);
      return;
    }
    this.setRemoteSdp_(message);
    this.doAnswer_();
  } else {
    if (message.type === "answer" && this.isInitiator_) {
      if (this.pc_.signalingState !== "have-local-offer") {
        trace("ERROR: remote answer received in unexpected state: " + this.pc_.signalingState);
        return;
      }
      this.setRemoteSdp_(message);
    } else {
      if (message.type === "candidate") {
        var candidate = new RTCIceCandidate({sdpMLineIndex:message.label, candidate:message.candidate});
        this.recordIceCandidate_("Remote", candidate);
        this.pc_.addIceCandidate(candidate).then(trace.bind(null, "Remote candidate added successfully.")).catch(this.onError_.bind(this, "addIceCandidate"));
      } else {
        trace("WARNING: unexpected message: " + JSON.stringify(message));
      }
    }
  }
};
PeerConnectionClient.prototype.drainMessageQueue_ = function() {
  if (!this.pc_ || !this.started_ || !this.hasRemoteSdp_) {
    return;
  }
  for (var i = 0, len = this.messageQueue_.length; i < len; i++) {
    this.processSignalingMessage_(this.messageQueue_[i]);
  }
  this.messageQueue_ = [];
};
PeerConnectionClient.prototype.onIceCandidate_ = function(event) {
  if (event.candidate) {
    if (this.filterIceCandidate_(event.candidate)) {
      var message = {type:"candidate", label:event.candidate.sdpMLineIndex, id:event.candidate.sdpMid, candidate:event.candidate.candidate};
      if (this.onsignalingmessage) {
        this.onsignalingmessage(message);
      }
      this.recordIceCandidate_("Local", event.candidate);
    }
  } else {
    trace("End of candidates.");
  }
};
PeerConnectionClient.prototype.onSignalingStateChanged_ = function() {
  if (!this.pc_) {
    return;
  }
  trace("Signaling state changed to: " + this.pc_.signalingState);
  if (this.onsignalingstatechange) {
    this.onsignalingstatechange();
  }
};
PeerConnectionClient.prototype.onIceConnectionStateChanged_ = function() {
  if (!this.pc_) {
    return;
  }
  trace("ICE connection state changed to: " + this.pc_.iceConnectionState);
  if (this.pc_.iceConnectionState === "completed") {
    trace("ICE complete time: " + (window.performance.now() - this.startTime_).toFixed(0) + "ms.");
  }
  if (this.oniceconnectionstatechange) {
    this.oniceconnectionstatechange();
  }
};
PeerConnectionClient.prototype.filterIceCandidate_ = function(candidateObj) {
  var candidateStr = candidateObj.candidate;
  if (candidateStr.indexOf("tcp") !== -1) {
    return false;
  }
  if (this.params_.peerConnectionConfig.iceTransports === "relay" && iceCandidateType(candidateStr) !== "relay") {
    return false;
  }
  return true;
};
PeerConnectionClient.prototype.recordIceCandidate_ = function(location, candidateObj) {
  if (this.onnewicecandidate) {
    this.onnewicecandidate(location, candidateObj.candidate);
  }
};
PeerConnectionClient.prototype.onRemoteStreamAdded_ = function(event) {
  if (this.onremotestreamadded) {
    this.onremotestreamadded(event.streams[0]);
  }
};
PeerConnectionClient.prototype.onError_ = function(tag, error) {
  if (this.onerror) {
    this.onerror(tag + ": " + error.toString());
  }
};
var RemoteWebSocket = function(wssUrl, wssPostUrl) {
  this.wssUrl_ = wssUrl;
  apprtc.windowPort.addMessageListener(this.handleMessage_.bind(this));
  this.sendMessage_({action:Constants.WS_ACTION, wsAction:Constants.WS_CREATE_ACTION, wssUrl:wssUrl, wssPostUrl:wssPostUrl});
  this.readyState = WebSocket.CONNECTING;
};
RemoteWebSocket.prototype.sendMessage_ = function(message) {
  apprtc.windowPort.sendMessage(message);
};
RemoteWebSocket.prototype.send = function(data) {
  if (this.readyState !== WebSocket.OPEN) {
    throw "Web socket is not in OPEN state: " + this.readyState;
  }
  this.sendMessage_({action:Constants.WS_ACTION, wsAction:Constants.WS_SEND_ACTION, data:data});
};
RemoteWebSocket.prototype.close = function() {
  if (this.readyState === WebSocket.CLOSING || this.readyState === WebSocket.CLOSED) {
    return;
  }
  this.readyState = WebSocket.CLOSING;
  this.sendMessage_({action:Constants.WS_ACTION, wsAction:Constants.WS_CLOSE_ACTION});
};
RemoteWebSocket.prototype.handleMessage_ = function(message) {
  if (message.action === Constants.WS_ACTION && message.wsAction === Constants.EVENT_ACTION) {
    if (message.wsEvent === Constants.WS_EVENT_ONOPEN) {
      this.readyState = WebSocket.OPEN;
      if (this.onopen) {
        this.onopen();
      }
    } else {
      if (message.wsEvent === Constants.WS_EVENT_ONCLOSE) {
        this.readyState = WebSocket.CLOSED;
        if (this.onclose) {
          this.onclose(message.data);
        }
      } else {
        if (message.wsEvent === Constants.WS_EVENT_ONERROR) {
          if (this.onerror) {
            this.onerror(message.data);
          }
        } else {
          if (message.wsEvent === Constants.WS_EVENT_ONMESSAGE) {
            if (this.onmessage) {
              this.onmessage(message.data);
            }
          } else {
            if (message.wsEvent === Constants.WS_EVENT_SENDERROR) {
              if (this.onsenderror) {
                this.onsenderror(message.data);
              }
              trace("ERROR: web socket send failed: " + message.data);
            }
          }
        }
      }
    }
  }
};
var RoomSelection = function(roomSelectionDiv, uiConstants, recentRoomsKey, setupCompletedCallback) {
  this.roomSelectionDiv_ = roomSelectionDiv;
  this.setupCompletedCallback_ = setupCompletedCallback;
  this.roomIdInput_ = this.roomSelectionDiv_.querySelector(uiConstants.roomSelectionInput);
  this.roomIdInputLabel_ = this.roomSelectionDiv_.querySelector(uiConstants.roomSelectionInputLabel);
  this.roomJoinButton_ = this.roomSelectionDiv_.querySelector(uiConstants.roomSelectionJoinButton);
  this.roomRandomButton_ = this.roomSelectionDiv_.querySelector(uiConstants.roomSelectionRandomButton);
  this.roomRecentList_ = this.roomSelectionDiv_.querySelector(uiConstants.roomSelectionRecentList);
  this.roomIdInput_.value = randomString(9);
  this.onRoomIdInput_();
  this.roomIdInputListener_ = this.onRoomIdInput_.bind(this);
  this.roomIdInput_.addEventListener("input", this.roomIdInputListener_, false);
  this.roomIdKeyupListener_ = this.onRoomIdKeyPress_.bind(this);
  this.roomIdInput_.addEventListener("keyup", this.roomIdKeyupListener_, false);
  this.roomRandomButtonListener_ = this.onRandomButton_.bind(this);
  this.roomRandomButton_.addEventListener("click", this.roomRandomButtonListener_, false);
  this.roomJoinButtonListener_ = this.onJoinButton_.bind(this);
  this.roomJoinButton_.addEventListener("click", this.roomJoinButtonListener_, false);
  this.onRoomSelected = null;
  this.recentlyUsedList_ = new RoomSelection.RecentlyUsedList(recentRoomsKey);
  this.startBuildingRecentRoomList_();
};
RoomSelection.matchRandomRoomPattern = function(input) {
  return input.match(/^\d{9}$/) !== null;
};
RoomSelection.prototype.removeEventListeners = function() {
  this.roomIdInput_.removeEventListener("input", this.roomIdInputListener_);
  this.roomIdInput_.removeEventListener("keyup", this.roomIdKeyupListener_);
  this.roomRandomButton_.removeEventListener("click", this.roomRandomButtonListener_);
  this.roomJoinButton_.removeEventListener("click", this.roomJoinButtonListener_);
};
RoomSelection.prototype.startBuildingRecentRoomList_ = function() {
  this.recentlyUsedList_.getRecentRooms().then(function(recentRooms) {
    this.buildRecentRoomList_(recentRooms);
    if (this.setupCompletedCallback_) {
      this.setupCompletedCallback_();
    }
  }.bind(this)).catch(function(error) {
    trace("Error building recent rooms list: " + error.message);
  }.bind(this));
};
RoomSelection.prototype.buildRecentRoomList_ = function(recentRooms) {
  var lastChild = this.roomRecentList_.lastChild;
  while (lastChild) {
    this.roomRecentList_.removeChild(lastChild);
    lastChild = this.roomRecentList_.lastChild;
  }
  for (var i = 0; i < recentRooms.length; ++i) {
    var li = document.createElement("li");
    var href = document.createElement("a");
    var linkText = document.createTextNode(recentRooms[i]);
    href.appendChild(linkText);
    href.href = location.origin + "/r/" + encodeURIComponent(recentRooms[i]);
    li.appendChild(href);
    this.roomRecentList_.appendChild(li);
    href.addEventListener("click", this.makeRecentlyUsedClickHandler_(recentRooms[i]).bind(this), false);
  }
};
RoomSelection.prototype.onRoomIdInput_ = function() {
  var room = this.roomIdInput_.value;
  var valid = room.length >= 5;
  var re = /^([a-zA-Z0-9-_]+)+$/;
  valid = valid && re.exec(room);
  if (valid) {
    this.roomJoinButton_.disabled = false;
    this.roomIdInput_.classList.remove("invalid");
    this.roomIdInputLabel_.classList.add("hidden");
  } else {
    this.roomJoinButton_.disabled = true;
    this.roomIdInput_.classList.add("invalid");
    this.roomIdInputLabel_.classList.remove("hidden");
  }
};
RoomSelection.prototype.onRoomIdKeyPress_ = function(event) {
  if (event.which !== 13 || this.roomJoinButton_.disabled) {
    return;
  }
  this.onJoinButton_();
};
RoomSelection.prototype.onRandomButton_ = function() {
  this.roomIdInput_.value = randomString(9);
  this.onRoomIdInput_();
};
RoomSelection.prototype.onJoinButton_ = function() {
  this.loadRoom_(this.roomIdInput_.value);
};
RoomSelection.prototype.makeRecentlyUsedClickHandler_ = function(roomName) {
  return function(e) {
    e.preventDefault();
    this.loadRoom_(roomName);
  };
};
RoomSelection.prototype.loadRoom_ = function(roomName) {
  this.recentlyUsedList_.pushRecentRoom(roomName);
  if (this.onRoomSelected) {
    this.onRoomSelected(roomName);
  }
};
RoomSelection.RecentlyUsedList = function(key) {
  this.LISTLENGTH_ = 10;
  this.RECENTROOMSKEY_ = key || "recentRooms";
  this.storage_ = new Storage;
};
RoomSelection.RecentlyUsedList.prototype.pushRecentRoom = function(roomId) {
  return new Promise(function(resolve, reject) {
    if (!roomId) {
      resolve();
      return;
    }
    this.getRecentRooms().then(function(recentRooms) {
      recentRooms = [roomId].concat(recentRooms);
      recentRooms = recentRooms.filter(function(value, index, self) {
        return self.indexOf(value) === index;
      });
      recentRooms = recentRooms.slice(0, this.LISTLENGTH_);
      this.storage_.setStorage(this.RECENTROOMSKEY_, JSON.stringify(recentRooms), function() {
        resolve();
      });
    }.bind(this)).catch(function(err) {
      reject(err);
    }.bind(this));
  }.bind(this));
};
RoomSelection.RecentlyUsedList.prototype.getRecentRooms = function() {
  return new Promise(function(resolve) {
    this.storage_.getStorage(this.RECENTROOMSKEY_, function(value) {
      var recentRooms = parseJSON(value);
      if (!recentRooms) {
        recentRooms = [];
      }
      resolve(recentRooms);
    });
  }.bind(this));
};
function mergeConstraints(cons1, cons2) {
  if (!cons1 || !cons2) {
    return cons1 || cons2;
  }
  var merged = cons1;
  for (var key in cons2) {
    merged[key] = cons2[key];
  }
  return merged;
}
function iceCandidateType(candidateStr) {
  return candidateStr.split(" ")[7];
}
function formatTypePreference(pref) {
  if (adapter.browserDetails.browser === "chrome") {
    switch(pref) {
      case 0:
        return "TURN/TLS";
      case 1:
        return "TURN/TCP";
      case 2:
        return "TURN/UDP";
      default:
        break;
    }
  } else {
    if (adapter.browserDetails.browser === "firefox") {
      switch(pref) {
        case 0:
          return "TURN/TCP";
        case 5:
          return "TURN/UDP";
        default:
          break;
      }
    }
  }
  return "";
}
function maybeSetOpusOptions(sdp, params) {
  if (params.opusStereo === "true") {
    sdp = setCodecParam(sdp, "opus/48000", "stereo", "1");
  } else {
    if (params.opusStereo === "false") {
      sdp = removeCodecParam(sdp, "opus/48000", "stereo");
    }
  }
  if (params.opusFec === "true") {
    sdp = setCodecParam(sdp, "opus/48000", "useinbandfec", "1");
  } else {
    if (params.opusFec === "false") {
      sdp = removeCodecParam(sdp, "opus/48000", "useinbandfec");
    }
  }
  if (params.opusDtx === "true") {
    sdp = setCodecParam(sdp, "opus/48000", "usedtx", "1");
  } else {
    if (params.opusDtx === "false") {
      sdp = removeCodecParam(sdp, "opus/48000", "usedtx");
    }
  }
  if (params.opusMaxPbr) {
    sdp = setCodecParam(sdp, "opus/48000", "maxplaybackrate", params.opusMaxPbr);
  }
  return sdp;
}
function maybeSetAudioSendBitRate(sdp, params) {
  if (!params.audioSendBitrate) {
    return sdp;
  }
  trace("Prefer audio send bitrate: " + params.audioSendBitrate);
  return preferBitRate(sdp, params.audioSendBitrate, "audio");
}
function maybeSetAudioReceiveBitRate(sdp, params) {
  if (!params.audioRecvBitrate) {
    return sdp;
  }
  trace("Prefer audio receive bitrate: " + params.audioRecvBitrate);
  return preferBitRate(sdp, params.audioRecvBitrate, "audio");
}
function maybeSetVideoSendBitRate(sdp, params) {
  if (!params.videoSendBitrate) {
    return sdp;
  }
  trace("Prefer video send bitrate: " + params.videoSendBitrate);
  return preferBitRate(sdp, params.videoSendBitrate, "video");
}
function maybeSetVideoReceiveBitRate(sdp, params) {
  if (!params.videoRecvBitrate) {
    return sdp;
  }
  trace("Prefer video receive bitrate: " + params.videoRecvBitrate);
  return preferBitRate(sdp, params.videoRecvBitrate, "video");
}
function preferBitRate(sdp, bitrate, mediaType) {
  var sdpLines = sdp.split("\r\n");
  var mLineIndex = findLine(sdpLines, "m=", mediaType);
  if (mLineIndex === null) {
    trace("Failed to add bandwidth line to sdp, as no m-line found");
    return sdp;
  }
  var nextMLineIndex = findLineInRange(sdpLines, mLineIndex + 1, -1, "m=");
  if (nextMLineIndex === null) {
    nextMLineIndex = sdpLines.length;
  }
  var cLineIndex = findLineInRange(sdpLines, mLineIndex + 1, nextMLineIndex, "c=");
  if (cLineIndex === null) {
    trace("Failed to add bandwidth line to sdp, as no c-line found");
    return sdp;
  }
  var bLineIndex = findLineInRange(sdpLines, cLineIndex + 1, nextMLineIndex, "b=AS");
  if (bLineIndex) {
    sdpLines.splice(bLineIndex, 1);
  }
  var bwLine = "b=AS:" + bitrate;
  sdpLines.splice(cLineIndex + 1, 0, bwLine);
  sdp = sdpLines.join("\r\n");
  return sdp;
}
function maybeSetVideoSendInitialBitRate(sdp, params) {
  var initialBitrate = parseInt(params.videoSendInitialBitrate);
  if (!initialBitrate) {
    return sdp;
  }
  var maxBitrate = parseInt(initialBitrate);
  var bitrate = parseInt(params.videoSendBitrate);
  if (bitrate) {
    if (initialBitrate > bitrate) {
      trace("Clamping initial bitrate to max bitrate of " + bitrate + " kbps.");
      initialBitrate = bitrate;
      params.videoSendInitialBitrate = initialBitrate;
    }
    maxBitrate = bitrate;
  }
  var sdpLines = sdp.split("\r\n");
  var mLineIndex = findLine(sdpLines, "m=", "video");
  if (mLineIndex === null) {
    trace("Failed to find video m-line");
    return sdp;
  }
  var videoMLine = sdpLines[mLineIndex];
  var pattern = new RegExp("m=video\\s\\d+\\s[A-Z/]+\\s");
  var sendPayloadType = videoMLine.split(pattern)[1].split(" ")[0];
  var fmtpLine = sdpLines[findLine(sdpLines, "a=rtpmap", sendPayloadType)];
  var codecName = fmtpLine.split("a=rtpmap:" + sendPayloadType)[1].split("/")[0];
  var codec = params.videoSendCodec || codecName;
  sdp = setCodecParam(sdp, codec, "x-google-min-bitrate", params.videoSendInitialBitrate.toString());
  sdp = setCodecParam(sdp, codec, "x-google-max-bitrate", maxBitrate.toString());
  return sdp;
}
function removePayloadTypeFromMline(mLine, payloadType) {
  mLine = mLine.split(" ");
  for (var i = 0; i < mLine.length; ++i) {
    if (mLine[i] === payloadType.toString()) {
      mLine.splice(i, 1);
    }
  }
  return mLine.join(" ");
}
function removeCodecByName(sdpLines, codec) {
  var index = findLine(sdpLines, "a=rtpmap", codec);
  if (index === null) {
    return sdpLines;
  }
  var payloadType = getCodecPayloadTypeFromLine(sdpLines[index]);
  sdpLines.splice(index, 1);
  var mLineIndex = findLine(sdpLines, "m=", "video");
  if (mLineIndex === null) {
    return sdpLines;
  }
  sdpLines[mLineIndex] = removePayloadTypeFromMline(sdpLines[mLineIndex], payloadType);
  return sdpLines;
}
function removeCodecByPayloadType(sdpLines, payloadType) {
  var index = findLine(sdpLines, "a=rtpmap", payloadType.toString());
  if (index === null) {
    return sdpLines;
  }
  sdpLines.splice(index, 1);
  var mLineIndex = findLine(sdpLines, "m=", "video");
  if (mLineIndex === null) {
    return sdpLines;
  }
  sdpLines[mLineIndex] = removePayloadTypeFromMline(sdpLines[mLineIndex], payloadType);
  return sdpLines;
}
function maybeRemoveVideoFec(sdp, params) {
  if (params.videoFec !== "false") {
    return sdp;
  }
  var sdpLines = sdp.split("\r\n");
  var index = findLine(sdpLines, "a=rtpmap", "red");
  if (index === null) {
    return sdp;
  }
  var redPayloadType = getCodecPayloadTypeFromLine(sdpLines[index]);
  sdpLines = removeCodecByPayloadType(sdpLines, redPayloadType);
  sdpLines = removeCodecByName(sdpLines, "ulpfec");
  index = findLine(sdpLines, "a=fmtp", redPayloadType.toString());
  if (index === null) {
    return sdp;
  }
  var fmtpLine = parseFmtpLine(sdpLines[index]);
  var rtxPayloadType = fmtpLine.pt;
  if (rtxPayloadType === null) {
    return sdp;
  }
  sdpLines.splice(index, 1);
  sdpLines = removeCodecByPayloadType(sdpLines, rtxPayloadType);
  return sdpLines.join("\r\n");
}
function maybePreferAudioSendCodec(sdp, params) {
  return maybePreferCodec(sdp, "audio", "send", params.audioSendCodec);
}
function maybePreferAudioReceiveCodec(sdp, params) {
  return maybePreferCodec(sdp, "audio", "receive", params.audioRecvCodec);
}
function maybePreferVideoSendCodec(sdp, params) {
  return maybePreferCodec(sdp, "video", "send", params.videoSendCodec);
}
function maybePreferVideoReceiveCodec(sdp, params) {
  return maybePreferCodec(sdp, "video", "receive", params.videoRecvCodec);
}
function maybePreferCodec(sdp, type, dir, codec) {
  var str = type + " " + dir + " codec";
  if (!codec) {
    trace("No preference on " + str + ".");
    return sdp;
  }
  trace("Prefer " + str + ": " + codec);
  var sdpLines = sdp.split("\r\n");
  var mLineIndex = findLine(sdpLines, "m=", type);
  if (mLineIndex === null) {
    return sdp;
  }
  var payload = null;
  for (var i = sdpLines.length - 1; i >= 0; --i) {
    var index = findLineInRange(sdpLines, i, 0, "a=rtpmap", codec, "desc");
    if (index !== null) {
      i = index;
      payload = getCodecPayloadTypeFromLine(sdpLines[index]);
      if (payload) {
        sdpLines[mLineIndex] = setDefaultCodec(sdpLines[mLineIndex], payload);
      }
    } else {
      break;
    }
  }
  sdp = sdpLines.join("\r\n");
  return sdp;
}
function setCodecParam(sdp, codec, param, value) {
  var sdpLines = sdp.split("\r\n");
  var fmtpLineIndex = findFmtpLine(sdpLines, codec);
  var fmtpObj = {};
  if (fmtpLineIndex === null) {
    var index = findLine(sdpLines, "a=rtpmap", codec);
    if (index === null) {
      return sdp;
    }
    var payload = getCodecPayloadTypeFromLine(sdpLines[index]);
    fmtpObj.pt = payload.toString();
    fmtpObj.params = {};
    fmtpObj.params[param] = value;
    sdpLines.splice(index + 1, 0, writeFmtpLine(fmtpObj));
  } else {
    fmtpObj = parseFmtpLine(sdpLines[fmtpLineIndex]);
    fmtpObj.params[param] = value;
    sdpLines[fmtpLineIndex] = writeFmtpLine(fmtpObj);
  }
  sdp = sdpLines.join("\r\n");
  return sdp;
}
function removeCodecParam(sdp, codec, param) {
  var sdpLines = sdp.split("\r\n");
  var fmtpLineIndex = findFmtpLine(sdpLines, codec);
  if (fmtpLineIndex === null) {
    return sdp;
  }
  var map = parseFmtpLine(sdpLines[fmtpLineIndex]);
  delete map.params[param];
  var newLine = writeFmtpLine(map);
  if (newLine === null) {
    sdpLines.splice(fmtpLineIndex, 1);
  } else {
    sdpLines[fmtpLineIndex] = newLine;
  }
  sdp = sdpLines.join("\r\n");
  return sdp;
}
function parseFmtpLine(fmtpLine) {
  var fmtpObj = {};
  var spacePos = fmtpLine.indexOf(" ");
  var keyValues = fmtpLine.substring(spacePos + 1).split(";");
  var pattern = new RegExp("a=fmtp:(\\d+)");
  var result = fmtpLine.match(pattern);
  if (result && result.length === 2) {
    fmtpObj.pt = result[1];
  } else {
    return null;
  }
  var params = {};
  for (var i = 0; i < keyValues.length; ++i) {
    var pair = keyValues[i].split("=");
    if (pair.length === 2) {
      params[pair[0]] = pair[1];
    }
  }
  fmtpObj.params = params;
  return fmtpObj;
}
function writeFmtpLine(fmtpObj) {
  if (!fmtpObj.hasOwnProperty("pt") || !fmtpObj.hasOwnProperty("params")) {
    return null;
  }
  var pt = fmtpObj.pt;
  var params = fmtpObj.params;
  var keyValues = [];
  var i = 0;
  for (var key in params) {
    keyValues[i] = key + "=" + params[key];
    ++i;
  }
  if (i === 0) {
    return null;
  }
  return "a=fmtp:" + pt.toString() + " " + keyValues.join(";");
}
function findFmtpLine(sdpLines, codec) {
  var payload = getCodecPayloadType(sdpLines, codec);
  return payload ? findLine(sdpLines, "a=fmtp:" + payload.toString()) : null;
}
function findLine(sdpLines, prefix, substr) {
  return findLineInRange(sdpLines, 0, -1, prefix, substr);
}
function findLineInRange(sdpLines, startLine, endLine, prefix, substr, direction) {
  if (direction === undefined) {
    direction = "asc";
  }
  direction = direction || "asc";
  if (direction === "asc") {
    var realEndLine = endLine !== -1 ? endLine : sdpLines.length;
    for (var i = startLine; i < realEndLine; ++i) {
      if (sdpLines[i].indexOf(prefix) === 0) {
        if (!substr || sdpLines[i].toLowerCase().indexOf(substr.toLowerCase()) !== -1) {
          return i;
        }
      }
    }
  } else {
    var realStartLine = startLine !== -1 ? startLine : sdpLines.length - 1;
    for (var j = realStartLine; j >= 0; --j) {
      if (sdpLines[j].indexOf(prefix) === 0) {
        if (!substr || sdpLines[j].toLowerCase().indexOf(substr.toLowerCase()) !== -1) {
          return j;
        }
      }
    }
  }
  return null;
}
function getCodecPayloadType(sdpLines, codec) {
  var index = findLine(sdpLines, "a=rtpmap", codec);
  return index ? getCodecPayloadTypeFromLine(sdpLines[index]) : null;
}
function getCodecPayloadTypeFromLine(sdpLine) {
  var pattern = new RegExp("a=rtpmap:(\\d+) [a-zA-Z0-9-]+\\/\\d+");
  var result = sdpLine.match(pattern);
  return result && result.length === 2 ? result[1] : null;
}
function setDefaultCodec(mLine, payload) {
  var elements = mLine.split(" ");
  var newLine = elements.slice(0, 3);
  newLine.push(payload);
  for (var i = 3; i < elements.length; i++) {
    if (elements[i] !== payload) {
      newLine.push(elements[i]);
    }
  }
  return newLine.join(" ");
}
;var SignalingChannel = function(wssUrl, wssPostUrl) {
  this.wssUrl_ = wssUrl;
  this.wssPostUrl_ = wssPostUrl;
  this.roomId_ = null;
  this.clientId_ = null;
  this.websocket_ = null;
  this.registered_ = false;
  this.onerror = null;
  this.onmessage = null;
};
SignalingChannel.prototype.open = function() {
  if (this.websocket_) {
    trace("ERROR: SignalingChannel has already opened.");
    return;
  }
  trace("Opening signaling channel.");
  return new Promise(function(resolve, reject) {
    if (isChromeApp()) {
      this.websocket_ = new RemoteWebSocket(this.wssUrl_, this.wssPostUrl_);
    } else {
      this.websocket_ = new WebSocket(this.wssUrl_, 'apprtc.esegece.com');
    }
    this.websocket_.onopen = function() {
      trace("Signaling channel opened.");
      this.websocket_.onerror = function() {
        trace("Signaling channel error.");
      };
      this.websocket_.onclose = function(event) {
        trace("Channel closed with code:" + event.code + " reason:" + event.reason);
        this.websocket_ = null;
        this.registered_ = false;
      };
      if (this.clientId_ && this.roomId_) {
        this.register(this.roomId_, this.clientId_);
      }
      resolve();
    }.bind(this);
    this.websocket_.onmessage = function(event) {
      trace("WSS->C: " + event.data);
      var message = parseJSON(event.data);
      if (!message) {
        trace("Failed to parse WSS message: " + event.data);
        return;
      }
      if (message.error) {
        trace("Signaling server error message: " + message.error);
        return;
      }
      this.onmessage(message.msg);
    }.bind(this);
    this.websocket_.onerror = function() {
      reject(Error("WebSocket error."));
    };
  }.bind(this));
};
SignalingChannel.prototype.register = function(roomId, clientId) {
  if (this.registered_) {
    trace("ERROR: SignalingChannel has already registered.");
    return;
  }
  this.roomId_ = roomId;
  this.clientId_ = clientId;
  if (!this.roomId_) {
    trace("ERROR: missing roomId.");
  }
  if (!this.clientId_) {
    trace("ERROR: missing clientId.");
  }
  if (!this.websocket_ || this.websocket_.readyState !== WebSocket.OPEN) {
    trace("WebSocket not open yet; saving the IDs to register later.");
    return;
  }
  trace("Registering signaling channel.");
  var registerMessage = {cmd:"register", roomid:this.roomId_, clientid:this.clientId_};
  this.websocket_.send(JSON.stringify(registerMessage));
  this.registered_ = true;
  trace("Signaling channel registered.");
};
SignalingChannel.prototype.close = function(async) {
  if (this.websocket_) {
    this.websocket_.close();
    this.websocket_ = null;
  }
  if (!this.clientId_ || !this.roomId_) {
    return;
  }
  var path = this.getWssPostUrl();
  return sendUrlRequest("DELETE", path, async).catch(function(error) {
    trace("Error deleting web socket connection: " + error.message);
  }.bind(this)).then(function() {
    this.clientId_ = null;
    this.roomId_ = null;
    this.registered_ = false;
  }.bind(this));
};
SignalingChannel.prototype.send = function(message) {
  if (!this.roomId_ || !this.clientId_) {
    trace("ERROR: SignalingChannel has not registered.");
    return;
  }
  trace("C->WSS: " + message);
  var wssMessage = {cmd:"send", msg:message};
  var msgString = JSON.stringify(wssMessage);
  if (this.websocket_ && this.websocket_.readyState === WebSocket.OPEN) {
    this.websocket_.send(msgString);
  } else {
    var path = this.getWssPostUrl();
    var xhr = new XMLHttpRequest;
    xhr.open("POST", path, true);
    xhr.send(wssMessage.msg);
  }
};
SignalingChannel.prototype.getWssPostUrl = function() {
  return this.wssPostUrl_ + "/" + this.roomId_ + "/" + this.clientId_;
};
function extractStatAsInt(stats, statObj, statName) {
  var str = extractStat(stats, statObj, statName);
  if (str) {
    var val = parseInt(str);
    if (val !== -1) {
      return val;
    }
  }
  return null;
}
function extractStat(stats, statObj, statName) {
  var report = getStatsReport(stats, statObj, statName);
  if (report && report[statName] !== -1) {
    return report[statName];
  }
  return null;
}
function getStatsReport(stats, statObj, statName, statVal) {
  var result = null;
  if (stats) {
    stats.forEach(function(report, stat) {
      if (report.type === statObj) {
        var found = true;
        if (statName) {
          var val = statName === "id" ? report.id : report[statName];
          found = statVal !== undefined ? val === statVal : val;
        }
        if (found) {
          result = report;
        }
      }
    });
  }
  return result;
}
function enumerateStats(stats, localTrackIds, remoteTrackIds) {
  var statsObject = {audio:{local:{audioLevel:0.0, bytesSent:0, clockRate:0, codecId:"", mimeType:"", packetsSent:0, payloadType:0, timestamp:0.0, trackId:"", transportId:""}, remote:{audioLevel:0.0, bytesReceived:0, clockRate:0, codecId:"", fractionLost:0, jitter:0, mimeType:"", packetsLost:0, packetsReceived:0, payloadType:0, timestamp:0.0, trackId:"", transportId:""}}, video:{local:{bytesSent:0, clockRate:0, codecId:"", firCount:0, framesEncoded:0, frameHeight:0, framesSent:0, frameWidth:0, nackCount:0, 
  packetsSent:0, payloadType:0, pliCount:0, qpSum:0, timestamp:0.0, trackId:"", transportId:""}, remote:{bytesReceived:0, clockRate:0, codecId:"", firCount:0, fractionLost:0, frameHeight:0, framesDecoded:0, framesDropped:0, framesReceived:0, frameWidth:0, nackCount:0, packetsLost:0, packetsReceived:0, payloadType:0, pliCount:0, qpSum:0, timestamp:0.0, trackId:"", transportId:""}}, connection:{availableOutgoingBitrate:0, bytesReceived:0, bytesSent:0, consentRequestsSent:0, currentRoundTripTime:0.0, 
  localCandidateId:"", localCandidateType:"", localIp:"", localPort:0, localPriority:0, localProtocol:"", remoteCandidateId:"", remoteCandidateType:"", remoteIp:"", remotePort:0, remotePriority:0, remoteProtocol:"", requestsReceived:0, requestsSent:0, responsesReceived:0, responsesSent:0, timestamp:0.0, totalRoundTripTime:0.0}};
  if (stats) {
    stats.forEach(function(report, stat) {
      switch(report.type) {
        case "outbound-rtp":
          if (report.hasOwnProperty("trackId")) {
            if (report.trackId.indexOf(localTrackIds.audio) !== -1) {
              statsObject.audio.local.bytesSent = report.bytesSent;
              statsObject.audio.local.codecId = report.codecId;
              statsObject.audio.local.packetsSent = report.packetsSent;
              statsObject.audio.local.timestamp = report.timestamp;
              statsObject.audio.local.trackId = report.trackId;
              statsObject.audio.local.transportId = report.transportId;
            }
            if (report.trackId.indexOf(localTrackIds.video) !== -1) {
              statsObject.video.local.bytesSent = report.bytesSent;
              statsObject.video.local.codecId = report.codecId;
              statsObject.video.local.firCount = report.firCount;
              statsObject.video.local.framesEncoded = report.frameEncoded;
              statsObject.video.local.framesSent = report.framesSent;
              statsObject.video.local.packetsSent = report.packetsSent;
              statsObject.video.local.pliCount = report.pliCount;
              statsObject.video.local.qpSum = report.qpSum;
              statsObject.video.local.timestamp = report.timestamp;
              statsObject.video.local.trackId = report.trackId;
              statsObject.video.local.transportId = report.transportId;
            }
          }
          break;
        case "inbound-rtp":
          if (report.hasOwnProperty("trackId")) {
            if (report.trackId.indexOf(remoteTrackIds.audio) !== -1) {
              statsObject.audio.remote.bytesReceived = report.bytesReceived;
              statsObject.audio.remote.codecId = report.codecId;
              statsObject.audio.remote.fractionLost = report.fractionLost;
              statsObject.audio.remote.jitter = report.jitter;
              statsObject.audio.remote.packetsLost = report.packetsLost;
              statsObject.audio.remote.packetsReceived = report.packetsReceived;
              statsObject.audio.remote.timestamp = report.timestamp;
              statsObject.audio.remote.trackId = report.trackId;
              statsObject.audio.remote.transportId = report.transportId;
            }
            if (report.trackId.indexOf(remoteTrackIds.video) !== -1) {
              statsObject.video.remote.bytesReceived = report.bytesReceived;
              statsObject.video.remote.codecId = report.codecId;
              statsObject.video.remote.firCount = report.firCount;
              statsObject.video.remote.fractionLost = report.fractionLost;
              statsObject.video.remote.nackCount = report.nackCount;
              statsObject.video.remote.packetsLost = report.patsLost;
              statsObject.video.remote.packetsReceived = report.packetsReceived;
              statsObject.video.remote.pliCount = report.pliCount;
              statsObject.video.remote.qpSum = report.qpSum;
              statsObject.video.remote.timestamp = report.timestamp;
              statsObject.video.remote.trackId = report.trackId;
              statsObject.video.remote.transportId = report.transportId;
            }
          }
          break;
        case "candidate-pair":
          if (report.hasOwnProperty("availableOutgoingBitrate")) {
            statsObject.connection.availableOutgoingBitrate = report.availableOutgoingBitrate;
            statsObject.connection.bytesReceived = report.bytesReceived;
            statsObject.connection.bytesSent = report.bytesSent;
            statsObject.connection.consentRequestsSent = report.consentRequestsSent;
            statsObject.connection.currentRoundTripTime = report.currentRoundTripTime;
            statsObject.connection.localCandidateId = report.localCandidateId;
            statsObject.connection.remoteCandidateId = report.remoteCandidateId;
            statsObject.connection.requestsReceived = report.requestsReceived;
            statsObject.connection.requestsSent = report.requestsSent;
            statsObject.connection.responsesReceived = report.responsesReceived;
            statsObject.connection.responsesSent = report.responsesSent;
            statsObject.connection.timestamp = report.timestamp;
            statsObject.connection.totalRoundTripTime = report.totalRoundTripTime;
          }
          break;
        default:
          return;
      }
    }.bind());
    stats.forEach(function(report) {
      switch(report.type) {
        case "track":
          if (report.hasOwnProperty("trackIdentifier")) {
            if (report.trackIdentifier.indexOf(localTrackIds.video) !== -1) {
              statsObject.video.local.frameHeight = report.frameHeight;
              statsObject.video.local.framesSent = report.framesSent;
              statsObject.video.local.frameWidth = report.frameWidth;
            }
            if (report.trackIdentifier.indexOf(remoteTrackIds.video) !== -1) {
              statsObject.video.remote.frameHeight = report.frameHeight;
              statsObject.video.remote.framesDecoded = report.framesDecoded;
              statsObject.video.remote.framesDropped = report.framesDropped;
              statsObject.video.remote.framesReceived = report.framesReceived;
              statsObject.video.remote.frameWidth = report.frameWidth;
            }
            if (report.trackIdentifier.indexOf(localTrackIds.audio) !== -1) {
              statsObject.audio.local.audioLevel = report.audioLevel;
            }
            if (report.trackIdentifier.indexOf(remoteTrackIds.audio) !== -1) {
              statsObject.audio.remote.audioLevel = report.audioLevel;
            }
          }
          break;
        case "codec":
          if (report.hasOwnProperty("id")) {
            if (report.id.indexOf(statsObject.audio.local.codecId) !== -1) {
              statsObject.audio.local.clockRate = report.clockRate;
              statsObject.audio.local.mimeType = report.mimeType;
              statsObject.audio.local.payloadType = report.payloadType;
            }
            if (report.id.indexOf(statsObject.audio.remote.codecId) !== -1) {
              statsObject.audio.remote.clockRate = report.clockRate;
              statsObject.audio.remote.mimeType = report.mimeType;
              statsObject.audio.remote.payloadType = report.payloadType;
            }
            if (report.id.indexOf(statsObject.video.local.codecId) !== -1) {
              statsObject.video.local.clockRate = report.clockRate;
              statsObject.video.local.mimeType = report.mimeType;
              statsObject.video.local.payloadType = report.payloadType;
            }
            if (report.id.indexOf(statsObject.video.remote.codecId) !== -1) {
              statsObject.video.remote.clockRate = report.clockRate;
              statsObject.video.remote.mimeType = report.mimeType;
              statsObject.video.remote.payloadType = report.payloadType;
            }
          }
          break;
        case "local-candidate":
          if (report.hasOwnProperty("id")) {
            if (report.id.indexOf(statsObject.connection.localCandidateId) !== -1) {
              statsObject.connection.localIp = report.ip;
              statsObject.connection.localPort = report.port;
              statsObject.connection.localPriority = report.priority;
              statsObject.connection.localProtocol = report.protocol;
              statsObject.connection.localType = report.candidateType;
            }
          }
          break;
        case "remote-candidate":
          if (report.hasOwnProperty("id")) {
            if (report.id.indexOf(statsObject.connection.remoteCandidateId) !== -1) {
              statsObject.connection.remoteIp = report.ip;
              statsObject.connection.remotePort = report.port;
              statsObject.connection.remotePriority = report.priority;
              statsObject.connection.remoteProtocol = report.protocol;
              statsObject.connection.remoteType = report.candidateType;
            }
          }
          break;
        default:
          return;
      }
    }.bind());
  }
  return statsObject;
}
function computeRate(newReport, oldReport, statName) {
  var newVal = newReport[statName];
  var oldVal = oldReport ? oldReport[statName] : null;
  if (newVal === null || oldVal === null) {
    return null;
  }
  return (newVal - oldVal) / (newReport.timestamp - oldReport.timestamp) * 1000;
}
function computeBitrate(newReport, oldReport, statName) {
  return computeRate(newReport, oldReport, statName) * 8;
}
function computeE2EDelay(captureStart, remoteVideoCurrentTime) {
  if (!captureStart) {
    return null;
  }
  var nowNTP = Date.now() + 2208988800000;
  return nowNTP - captureStart - remoteVideoCurrentTime * 1000;
}
;var Storage = function() {
};
Storage.prototype.getStorage = function(key, callback) {
  if (isChromeApp()) {
    chrome.storage.local.get(key, function(values) {
      if (callback) {
        window.setTimeout(function() {
          callback(values[key]);
        }, 0);
      }
    });
  } else {
    var value = localStorage.getItem(key);
    if (callback) {
      window.setTimeout(function() {
        callback(value);
      }, 0);
    }
  }
};
Storage.prototype.setStorage = function(key, value, callback) {
  if (isChromeApp()) {
    var data = {};
    data[key] = value;
    chrome.storage.local.set(data, callback);
  } else {
    localStorage.setItem(key, value);
    if (callback) {
      window.setTimeout(callback, 0);
    }
  }
};
function $(selector) {
  return document.querySelector(selector);
}
function queryStringToDictionary(queryString) {
  var pairs = queryString.slice(1).split("&");
  var result = {};
  pairs.forEach(function(pair) {
    if (pair) {
      pair = pair.split("=");
      if (pair[0]) {
        result[pair[0]] = decodeURIComponent(pair[1] || "");
      }
    }
  });
  return result;
}
function sendAsyncUrlRequest(method, url, body) {
  return sendUrlRequest(method, url, true, body);
}
function sendUrlRequest(method, url, async, body) {
  return new Promise(function(resolve, reject) {  
    var xhr;
    var reportResults = function() {
      if (xhr.status !== 200) {
        reject(Error("Status=" + xhr.status + ", response=" + xhr.responseText));
        return;
      }
      resolve(xhr.responseText);
    };
    xhr = new XMLHttpRequest;
    if (async) {
      xhr.onreadystatechange = function() {
        if (xhr.readyState !== 4) {
          return;
        }
        reportResults();
      };
    }
    xhr.open(method, '/sgc/protocol/apprtc.esegece.com' + url, async);
    xhr.send(body);
    if (!async) {
      reportResults();
    }
  });
}
function requestIceServers(iceServerRequestUrl, iceTransports) {
  return new Promise(function(resolve, reject) {
    sendAsyncUrlRequest("POST", iceServerRequestUrl).then(function(response) {
      var iceServerRequestResponse = parseJSON(response);
      if (!iceServerRequestResponse) {
        reject(Error("Error parsing response JSON: " + response));
        return;
      }
      if (iceTransports !== "") {
        filterIceServersUrls(iceServerRequestResponse, iceTransports);
      }
      trace("Retrieved ICE server information.");
      resolve(iceServerRequestResponse.iceServers);
    }).catch(function(error) {
      reject(Error("ICE server request error: " + error.message));
      return;
    });
  });
}
function parseJSON(json) {
  try {
    return JSON.parse(json);
  } catch (e) {
    trace("Error parsing json: " + json);
  }
  return null;
}
function filterIceServersUrls(config, protocol) {
  var transport = "transport=" + protocol;
  var newIceServers = [];
  for (var i = 0; i < config.iceServers.length; ++i) {
    var iceServer = config.iceServers[i];
    var newUrls = [];
    for (var j = 0; j < iceServer.urls.length; ++j) {
      var url = iceServer.urls[j];
      if (url.indexOf(transport) !== -1) {
        newUrls.push(url);
      } else {
        if (url.indexOf("?transport=") === -1) {
          newUrls.push(url + "?" + transport);
        }
      }
    }
    if (newUrls.length !== 0) {
      iceServer.urls = newUrls;
      newIceServers.push(iceServer);
    }
  }
  config.iceServers = newIceServers;
}
function setUpFullScreen() {
  if (isChromeApp()) {
    document.cancelFullScreen = function() {
      chrome.app.window.current().restore();
    };
  } else {
    document.cancelFullScreen = document.webkitCancelFullScreen || document.mozCancelFullScreen || document.cancelFullScreen;
  }
  if (isChromeApp()) {
    document.body.requestFullScreen = function() {
      chrome.app.window.current().fullscreen();
    };
  } else {
    document.body.requestFullScreen = document.body.webkitRequestFullScreen || document.body.mozRequestFullScreen || document.body.requestFullScreen;
  }
  document.onfullscreenchange = document.onfullscreenchange || document.onwebkitfullscreenchange || document.onmozfullscreenchange;
}
function isFullScreen() {
  if (isChromeApp()) {
    return chrome.app.window.current().isFullscreen();
  }
  return !!(document.webkitIsFullScreen || document.mozFullScreen || document.isFullScreen);
}
function fullScreenElement() {
  return document.webkitFullScreenElement || document.webkitCurrentFullScreenElement || document.mozFullScreenElement || document.fullScreenElement;
}
function randomString(strLength) {
  var result = [];
  strLength = strLength || 5;
  var charSet = "0123456789";
  while (strLength--) {
    result.push(charSet.charAt(Math.floor(Math.random() * charSet.length)));
  }
  return result.join("");
}
function isChromeApp() {
  return typeof chrome !== "undefined" && typeof chrome.storage !== "undefined" && typeof chrome.storage.local !== "undefined";
}
function calculateFps(videoElement, decodedFrames, startTime, remoteOrLocal, callback) {
  var fps = 0;
  if (videoElement && typeof videoElement.webkitDecodedFrameCount !== undefined) {
    if (videoElement.readyState >= videoElement.HAVE_CURRENT_DATA) {
      var currentTime = (new Date).getTime();
      var deltaTime = (currentTime - startTime) / 1000;
      var startTimeToReturn = currentTime;
      fps = (videoElement.webkitDecodedFrameCount - decodedFrames) / deltaTime;
      callback(videoElement.webkitDecodedFrameCount, startTimeToReturn, remoteOrLocal);
    }
  }
  return parseInt(fps);
}
function trace(text) {
  if (text[text.length - 1] === "\n") {
    text = text.substring(0, text.length - 1);
  }
  if (window.performance) {
    var now = (window.performance.now() / 1000).toFixed(3);
    console.log(now + ": " + text);
  } else {
    console.log(text);
  }
}
;var apprtc = apprtc || {};
apprtc.windowPort = apprtc.windowPort || {};
(function() {
  var port_;
  apprtc.windowPort.sendMessage = function(message) {
    var port = getPort_();
    try {
      port.postMessage(message);
    } catch (ex) {
      trace("Error sending message via port: " + ex);
    }
  };
  apprtc.windowPort.addMessageListener = function(listener) {
    var port = getPort_();
    port.onMessage.addListener(listener);
  };
  var getPort_ = function() {
    if (!port_) {
      port_ = chrome.runtime.connect();
    }
    return port_;
  };
})();

