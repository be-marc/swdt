function(el,x,data){
  var map = this;
  var labels = map.layerManager._byGroup.Classified;
  var opacitySlider = new L.Control.opacitySlider();
               
               for (const prop in labels) {
               opacitySlider.setOpacityLayer(labels[prop]);
               }
               
               map.addControl(opacitySlider);}