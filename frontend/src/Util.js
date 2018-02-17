'strict';

var Svg = {
  svg: document.createElementNS('http://www.w3.org/2000/svg', 'svg'),

  createPoint: function(x, y) {
    const pt = this.svg.createSVGPoint();
    pt.x = x;
    pt.y = y;
    return pt;
  },

  identity: function() {
    return this.svg.createSVGMatrix();
  }
};



exports.localPoint = function(event) {
  const svg = event.target.closest('svg');
  const pt = Svg.createPoint(event.clientX, event.clientY);
  return pt.matrixTransform(svg.getScreenCTM().inverse());
};

exports.logRecordForce = function(r) {
  return function () {
    console.log(r);
  };
};

exports.loadImage = function(id) {
  return function(url) {
    return function() {
      document.getElementById(id).setAttributeNS('http://www.w3.org/1999/xlink', 'href', url);
    };
  };
};

exports.identityMatrix = Svg.identity

exports.translateMatrix = function(dx) {
  return function(dy) {
    return function(m) {
      return m.translate(dx, dy);
    };
  };
};

exports.rotateMatrix = function(angle) {
  return function(m) {
    return m.rotate(m);
  };
};


