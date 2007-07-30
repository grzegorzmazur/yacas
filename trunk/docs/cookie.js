
/*
WM_setCookie(), WM_readCookie(), WM_killCookie()
A set of functions that eases the pain of using cookies.

Source: Webmonkey Code Library
(http://www.hotwired.com/webmonkey/javascript/code_library/)

Author: Nadav Savio
Author Email: nadav@wired.com
*/


// This next little bit of code tests whether the user accepts cookies.
var WM_acceptsCookies = false;
if(document.cookie == '') {
    document.cookie = 'WM_acceptsCookies=yes'; // Try to set a cookie.
    if(document.cookie.indexOf('WM_acceptsCookies=yes') != -1) {
	WM_acceptsCookies = true; 
    }// If it succeeds, set variable
} else { // there was already a cookie
  WM_acceptsCookies = true;
}


function WM_setCookie (name, value, hours, path, domain, secure) {
    if (WM_acceptsCookies) { // Don't waste your time if the browser doesn't accept cookies.
	var not_NN2 = (navigator && navigator.appName 
		       && (navigator.appName == 'Netscape') 
		       && navigator.appVersion 
		       && (parseInt(navigator.appVersion) == 2))?false:true;

	if(hours && not_NN2) { // NN2 cannot handle Dates, so skip this part
	    if ( (typeof(hours) == 'string') && Date.parse(hours) ) { // already a Date string
		var numHours = hours;
	    } else if (typeof(hours) == 'number') { // calculate Date from number of hours
		var numHours = (new Date((new Date()).getTime() + hours*3600000)).toGMTString();
	    }
	}
	document.cookie = name + '=' + escape(value) + ((numHours)?(';expires=' + numHours):'') + ((path)?';path=' + path:'') + ((domain)?';domain=' + domain:'') + ((secure && (secure == true))?'; secure':''); // Set the cookie, adding any parameters that were specified.
    }
} // WM_setCookie


function WM_readCookie(name) {
    if(document.cookie == '') { // there's no cookie, so go no further
	return false; 
    } else { // there is a cookie
	var firstChar, lastChar;
	var theBigCookie = document.cookie;
	firstChar = theBigCookie.indexOf(name);	// find the start of 'name'
	var NN2Hack = firstChar + name.length;
	if((firstChar != -1) && (theBigCookie.charAt(NN2Hack) == '=')) { // if you found the cookie
	    firstChar += name.length + 1; // skip 'name' and '='
	    lastChar = theBigCookie.indexOf(';', firstChar); // Find the end of the value string (i.e. the next ';').
	    if(lastChar == -1) lastChar = theBigCookie.length;
	    return unescape(theBigCookie.substring(firstChar, lastChar));
	} else { // If there was no cookie of that name, return false.
	    return false;
	}
    }	
} // WM_readCookie

function WM_killCookie(name, path, domain) {
  var theValue = WM_readCookie(name); // We need the value to kill the cookie
  if(theValue) {
      document.cookie = name + '=' + theValue + '; expires=Fri, 13-Apr-1970 00:00:00 GMT' + ((path)?';path=' + path:'') + ((domain)?';domain=' + domain:''); // set an already-expired cookie
  }
} // WM_killCookie
