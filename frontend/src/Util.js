'strict';

exports.localPoint = function(event) {
  const svg = event.target.closest('svg');
  console.log(svg);
  const pt = svg.createSVGPoint();
  pt.x = event.clientX;
  pt.y = event.clientY;
  return pt.matrixTransform(svg.getScreenCTM().inverse());
};

exports.loadImage = function(id) {
  return function(url) {
    return function() {
      document.getElementById(id).setAttributeNS('http://www.w3.org/1999/xlink', 'href', url);
    };
  };
};
