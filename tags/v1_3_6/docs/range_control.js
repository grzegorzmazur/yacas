
var prevInit = window.onload;
window.onload = initSliders;

var scrollWidth = 320-4; // Same as in css file

function initSliders()
{
  if (prevInit) prevInit();

  var sliderReplacements = getElementsByClass("slider");

  for (var i = 0; i < sliderReplacements.length; i++)
  {
    var container = document.createElement("div");
    var leftSlider = document.createElement("div");
    var rightSlider = document.createElement("div");
    var inbetween = document.createElement("div");
    var textual =  document.createElement("div");

    var sliderReplacementID = sliderReplacements[i].getAttribute("id");

    if (sliderReplacementID != null || sliderReplacementID != "")
    {
      container.setAttribute("id", sliderReplacementID + "SliderContainer");
    }

    container.className = "sliderContainer";

    var lowValue = sliderReplacements[i].getAttribute("lowValue");
    if (lowValue == null) lowValue = "0";

    var highValue = sliderReplacements[i].getAttribute("highValue");
    if (highValue == null) highValue = "0";

    var variable = sliderReplacements[i].getAttribute("variable");
    if (variable == null) variable = "0";

    textual.innerHTML = lowValue+" &lt; "+variable+" &lt; "+highValue;

    var cursorPosition = getAbsolutePosition(sliderReplacements[i]);
    var left = cursorPosition[0];
    var top = cursorPosition[1];

    leftSlider.valueX = left;
    leftSlider.className = "sliderWidgetLeft";
    leftSlider.style.left = (leftSlider.valueX) + "px";
    leftSlider.style.top = (top) + "px";

    leftSlider.calculatePosition = calculatePosition;

    var right = parseInt(left)+scrollWidth;
    rightSlider.valueX = right;
    rightSlider.className = "sliderWidgetRight";
    rightSlider.style.left = (rightSlider.valueX) + "px";
    rightSlider.style.top = (top) + "px";

    rightSlider.calculatePosition = calculatePosition;

    inbetween.className = "sliderInbetween";
    inbetween.style.left = (leftSlider.valueX) + "px";
    inbetween.style.top = (top) + "px";
    inbetween.style.width = (rightSlider.valueX-leftSlider.valueX) + "px";

    container.appendChild(inbetween);
    container.appendChild(leftSlider);
    container.appendChild(rightSlider);

    sliderReplacements[i].parentNode.insertBefore(container, sliderReplacements[i]);
    sliderReplacements[i].parentNode.insertBefore(textual, sliderReplacements[i]);

    container.leftSlider  = leftSlider;
    container.inbetween   = inbetween;
    container.rightSlider = rightSlider;
    container.textual     = textual;

    container.variable  = variable;
    container.lowValue  = lowValue;
    container.highValue = highValue;

    updateTextual(container);

    attachEventListener(leftSlider, "mousedown", mousedownSlider, false);
    attachEventListener(rightSlider, "mousedown", mousedownSlider, false);
  }

  return true;
}

function mousedownSlider(event)
{
  if (typeof event == "undefined")
  {
    event = window.event;
  }

  var target = getEventTarget(event);
  while (target.className.indexOf("sliderWidget") < 0)
    target = target.parentNode;

  document.currentSlider = target;
  target.originX = event.clientX;

  attachEventListener(document, "mousemove", mousemoveSlider, false);
  attachEventListener(document, "mouseup", mouseupSlider, false);

  stopDefaultAction(event);

  return true;
}

function getAbsolutePosition(elem)
{
  var position = [0, 0];
  while (elem != null)
  {
    position[0] += elem.offsetLeft;
    position[1] += elem.offsetTop;
    elem = elem.offsetParent;
  }
  return position;
}

function mousemoveSlider(event)
{
  if (typeof event == "undefined")
  {
    event = window.event;
  }

  var slider = document.currentSlider;
  var sliderLeft = slider.valueX;
  var increment = 1;

  if (isNaN(sliderLeft))
  {
    sliderLeft = 0;
  }

  sliderLeft += event.clientX - slider.originX;

  var sliderWidth = slider.offsetWidth;

  var parentWidth = slider.parentNode.offsetWidth;

  var parentPosition = getAbsolutePosition(slider.parentNode);
  var leftPosition   = getAbsolutePosition(slider.parentNode.leftSlider );
  var rightPosition  = getAbsolutePosition(slider.parentNode.rightSlider);

  if (sliderLeft < parentPosition[0])
  {
    sliderLeft = parentPosition[0];
  }
  else if (sliderLeft > parentPosition[0] + parentWidth - sliderWidth)
  {
    sliderLeft = parentPosition[0] + parentWidth - sliderWidth;
  }
  else
  {
    slider.originX = event.clientX;
  }

  if (slider.className == "sliderWidgetLeft")
  {
    slider.parentNode.inbetween.style.left = sliderLeft+"px";
    slider.parentNode.inbetween.style.width = (rightPosition[0] - sliderLeft-sliderWidth)+"px";
    leftPosition[0] = sliderLeft;
  }
  else if (slider.className == "sliderWidgetRight")
  {
    var cursorPosition = leftPosition;
    slider.parentNode.inbetween.style.width = (sliderLeft-leftPosition[0]+sliderWidth)+"px";
    rightPosition[0] = sliderLeft;
  }

  slider.style.left = Math.round(sliderLeft / increment) * increment + "px";
  slider.valueX = sliderLeft;

  updateTextual(slider.parentNode);

  stopDefaultAction(event);
  return true;
}

function calculatePosition()
{
  var parentPosition = getAbsolutePosition(this.parentNode);
  var sliderPosition = getAbsolutePosition(this);
  var value;
  sliderPosition[0] -= 8;
  value = sliderPosition[0]-parentPosition[0];
  value  = value  * (this.parentNode.highValue-this.parentNode.lowValue);
  value = value / scrollWidth;
  value  = value + parseFloat(this.parentNode.lowValue);
  return value;
}

function updateTextual(container)
{
  if (container.textual)
  {
    var leftVal  = container.leftSlider.calculatePosition();
    var rightVal = container.rightSlider.calculatePosition();
    leftVal  = parseInt(""+(100*leftVal))/100;
    rightVal = parseInt(""+(100*rightVal))/100;
    container.textual.innerHTML = leftVal+" &lt; "+container.variable+" &lt; "+rightVal;
  }
}

function mouseupSlider()
{
  detachEventListener(document, "mousemove", mousemoveSlider, false);
  detachEventListener(document, "mouseup", mouseupSlider, false);

  return true;
}

function attachEventListener(target, eventType, functionRef, capture)
{
  if (typeof target.addEventListener != "undefined")
  {
    target.addEventListener(eventType, functionRef, capture);
  }
  else if (typeof target.attachEvent != "undefined")
  {
    target.attachEvent("on" + eventType, functionRef);
  }
  else
  {
    eventType = "on" + eventType;

    if (typeof target[eventType] == "function")
    {
      var oldListener = target[eventType];

      target[eventType] = function()
      {
        oldListener();

        return functionRef();
      }
    }
    else
    {
      target[eventType] = functionRef;
    }
  }

  return true;
}

function detachEventListener(target, eventType, functionRef, capture)
{
  if (typeof target.removeEventListener != "undefined")
  {
    target.removeEventListener(eventType, functionRef, capture)
  }
  else if (typeof target.detachEvent != "undefined")
  {
    target.detachEvent("on" + eventType, functionRef);
  }
  else
  {
    target["on" + eventType] = null;
  }

  return true;
}

function getEventTarget(event)
{
  var targetElement = null;

  if (typeof event.target != "undefined")
  {
    targetElement = event.target;
  }
  else
  {
    targetElement = event.srcElement;
  }
  while (targetElement.nodeType == 3 && targetElement.parentNode != null)
  {
    targetElement = targetElement.parentNode;
  }
  return targetElement;
}

function stopDefaultAction(event)
{
  event.returnValue = false;
  if (typeof event.preventDefault != "undefined")
  {
    event.preventDefault();
  }
  return true;
}

function getElementsByClass(classValue)
{
  var elementArray = new Array();
  var matchedArray = new Array();
  if (document.all)
  {
    elementArray = document.all;
  }
  else
  {
    elementArray = document.getElementsByTagName("*");
  }
  for (var i = 0; i < elementArray.length; i++)
  {
    if (elementArray[i].className == classValue)
    {
      matchedArray[matchedArray.length] = elementArray[i];
    }
  }
  return matchedArray;
}


