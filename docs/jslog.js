
/*
Copyright (c) 2005, Andre Lewis, andre@earthcode.com
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided 
that the following conditions are met:

    * Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
    * Neither the name of "Andre Lewis" nor the names of contributors to this software may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR 
IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND 
FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR 
CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL 
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, 
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN 
ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

/*
1. INSTALLATION: Include this file in every page:
	<script language="JavaScript" src="/scripts/jslog.js"></script>

2. USAGE: In a script block, use jslog.debug("My Message");
	- also jslog.info(), jslog.warning(), jslog.error()
	- jslog displays in the upper left corner. Double-click to expand the panel.

3. ENABLE/DISABLE: set config_enable=false; below to turn jslog display off.

4. MORE INFO/INSTRUCTIONS/UNCOMPRESSED VERSION: www.earthcode.com
V.7
*/
var config_enabled=true;
if(EC==null||typeof (EC)!="object"){
var EC=new Object();
}
if(EC.F==null||typeof (EC.F)!="object"){
EC.F=new Object();
}
if(EC.F["setCookie"]==null){
EC.F.setCookie=function(_1,_2){
var _3=_1+"="+escape(_2);
document.cookie=_3;
return null;
};
EC.F.getCookie=function(_4){
var _5=_4+"=";
var _6=document.cookie;
var _7=null;
if(_6.length>0){
offset=_6.indexOf(_5);
if(offset!=-1){
offset+=_5.length;
end=_6.indexOf(";",offset);
if(end==-1){
end=_6.length;
}
_7=unescape(_6.substring(offset,end));
}
}
return _7;
};
}
var jslog=new function(){
bPersistState=true;
var _8="ec_debug_logging";
var _9="jslog";
var _a=_8+"_"+_9;
var _b=config_enabled;
if(_b==false&&location.href.match(/enablejslog/)){
_b=true;
}
var _c=false;
var _d=0;
function $(o){
return document.getElementById(o);
}
function debug(_f){
logMsg("DEBUG",_f);
}
function info(msg){
logMsg("INFO",msg);
}
function warning(msg){
logMsg("WARN",msg);
}
function error(msg){
logMsg("ERROR",msg);
}
function toggleDisplay(){
var _13=$(_a+"_body");
if(_13.style.display=="none"){
_13.style.display="block";
}else{
_13.style.display="none";
}
if(bPersistState){
EC.F.setCookie(_9+"_visibility",_13.style.display);
}
}
function clearLog(){
$(_a+"_logDisplay").innerHTML="";
_d=0;
$(_a+"_handle").innerHTML=_d;
}
function enable(){
if(!_c){
initializeDisplay();
}
}
function text(_14){
$(_a+"_textArea").value=_14;
}
function getHTML(){
var _15=$(_a+"_idToInspect").value;
if(_15==""){
warning("Provide a non-blank id");
}else{
try{
var _16=$(_a+"_textArea").value=$(_15).innerHTML;
info(_15+" innerHTML is now in the text box below!");
}
catch(e){
error("Could not get innerHTML of id="+_15+": "+e.message);
}
EC.F.setCookie(_9+"_idToInspect",_15);
}
}
this.debug=debug;
this.info=info;
this.warning=warning;
this.error=error;
this.toggleDisplay=toggleDisplay;
this.clearLog=clearLog;
this.text=text;
this.enable=enable;
this.getHTML=getHTML;
function logMsg(_17,msg){
if(_b){
_d+=1;
$(_a+"_handle").innerHTML=_d;
var _19=$(_a+"_logDisplay");
if(_19.childNodes.length==0){
_19.appendChild(createDisplayRow(_17,msg));
}else{
_19.insertBefore(createDisplayRow(_17,msg),_19.childNodes[0]);
}
}
}
function createDisplayRow(_1a,_1b){
if(document.all){
var _1c="styleFloat";
}else{
var _1c="cssFloat";
}
var _1d=document.createElement("div");
if(_d/2==Math.floor(_d/2)){
_1d.style.backgroundColor="#FFF";
}else{
_1d.style.backgroundColor="#F6F6F6";
}
_1d.style.borderBottom="1px solid #AAA";
_1d.style.verticalAlign="top";
var _1e=document.createElement("div");
_1e.style.width="40px";
_1e.style.paddingLeft="3px";
_1e.style[_1c]="left";
if(_1a=="DEBUG"){
_1e.style.backgroundColor="#1515FF";
}else{
if(_1a=="INFO"){
_1e.style.backgroundColor="#10FF10";
}else{
if(_1a=="WARN"){
_1e.style.backgroundColor="yellow";
}else{
if(_1a=="ERROR"){
_1e.style.backgroundColor="#FF7070";
}
}
}
}
_1e.appendChild(document.createTextNode(_1a));
_1d.appendChild(_1e);
var _1f=document.createElement("span");
_1f.style.paddingLeft="3px";
_1f.style.paddingRight="8px";
_1e.style[_1c]="left";
_1f.appendChild(document.createTextNode(getCurrentTimeFormatted()));
_1d.appendChild(_1f);
_1d.appendChild(document.createTextNode(_1b));
var _20=document.createElement("div");
_20.style.clear="both";
_1d.appendChild(_20);
return _1d;
}
function getCurrentTimeFormatted(){
var now=new Date();
var _22=now.getHours();
var _23=now.getMinutes();
var _24=now.getSeconds();
var _25=""+((_22>12)?_22-12:_22);
if(_25=="0"){
_25=12;
}
_25+=((_23<10)?":0":":")+_23;
_25+=((_24<10)?":0":":")+_24;
_25+=(_22>=12)?" PM":" AM";
return _25;
}
if(_b){
initializeDisplay();
}
function initializeDisplay(){
if(!_c){
try{
var _26=2;
var _27=2;
var _28="none";
if(bPersistState){
try{
var _29=EC.F.getCookie(_9+"_position");
if(_29!=null){
var _2a=_29.split("|");
if(!isNaN(parseInt(_2a[0]))){
_27=_2a[0];
}
if(!isNaN(parseInt(_2a[1]))){
_26=_2a[1];
}
}
if(EC.F.getCookie(_9+"_visibility")=="block"){
_28="block";
}
}
catch(e){
}
}
var _2b=EC.F.getCookie(_9+"_idToInspect");
_2b=_2b==null?"":_2b;
document.write("<div id=\""+_a+"_container\" style=\"font-family:arial; color:black; font-size:9px; line-height:normal; letter-spacing: normal; position:absolute; z-index:10000;top:"+_26+"px; left:"+_27+"px; \">"+"<div id=\""+_a+"_handle\" style=\"cursor:move; position: absolute; background-color:#FFFFCC; border:1px solid #FF0400; color:black; padding:2px;\" ondblclick=\""+_9+".toggleDisplay()\">0</div>"+"<div id=\""+_a+"_body\" style=\"text-align:left; border:1px solid #FF0400; width:300px; position: absolute; top:20px; left:0px; background-color:white; display:"+_28+"\">"+"<div id=\""+_a+"_header\" style=\"height:10px; padding:2px; border-bottom:1px solid black; background-color:#FFFFCC;\">"+"<span id=\""+_a+"_clear\" style=\"color: blue;\" onclick=\""+_9+".clearLog()\">clear</span>"+"</div>"+"<div id=\""+_a+"_logDisplay\" style=\"height:240px; overflow:auto;\"></div>"+"<div id=\""+_a+"_footer\" style=\"padding-left:2px; border-top:1px solid black; background-color:#FFFFCC;\">"+"get html:<input id=\""+_a+"_idToInspect\" style=\"font-size:9px; height:18px;\" value=\""+_2b+"\" size=42/> <span id=\""+_a+"_go\" style=\"color: blue;\" onclick=\""+_9+".getHTML()\">go</span>"+"<textarea id=\""+_a+"_textArea\" style=\"width:99%; font-size:9px;\"></textarea>"+"</div></div></div></div>");
$(_a+"_clear").style.cursor="pointer";
$(_a+"_go").style.cursor="pointer";
if(window["Draggable"]!=null){
new Draggable(_a+"_container",{handle:_a+"_handle",revert:false,starteffect:false,endeffect:false});
if(bPersistState){
var _2c=new function(){
this.onStart=function(){
};
this.onEnd=function(s,o){
if(o.element.id==_a+"_container"){
var pos=Position.cumulativeOffset(o.element);
EC.F.setCookie(_9+"_position",+pos[0]+"|"+pos[1]);
}
};
};
Draggables.addObserver(_2c);
}
}else{
$(_a+"_handle").style.cursor="pointer";
}
_c=true;
}
catch(e){
alert("Code-level error initializing jslog: "+e.description);
}
}
}
};
debug=jslog.debug;

