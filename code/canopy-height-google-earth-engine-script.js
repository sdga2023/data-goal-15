var dataset = ee.Image('projects/glad/GLCLU2020/Forest_height_2020');
print(dataset)

var forestCanopyHeight = dataset;
var forestCanopyHeightVis = {
  min: 3,
  max: 30.0,
  palette: [
    'ffffff', 'fcd163', '99b718', '66a000', '3e8601', '207401', '056201',
    '004c00', '011301'
  ],
};
Map.setCenter(10, 0, 5);
Map.addLayer(forestCanopyHeight, forestCanopyHeightVis, 'Forest Canopy Height');

var thumbCanopyHeight = forestCanopyHeight.updateMask(forestCanopyHeight).getThumbURL({
  min: 3,
  max: 30,
  palette: [
    'ffffff', 'fcd163', '99b718', '66a000', '3e8601', '207401', '056201',
    '004c00', '011301'
  ],
  dimensions: 4096
})

print('Canopy height thumbnail', thumbCanopyHeight)