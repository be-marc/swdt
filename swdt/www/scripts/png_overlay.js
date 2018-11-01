function(el, x, data) {
  var map = this;
  var imageUrl = data.png;
  var imageBounds = data.thumb_extent;
  L.imageOverlay(imageUrl, imageBounds).addTo(map);
}