var wordcloud_draw = function(
  canvas_id, data_json, scale_size=1, font_family="Open Sans"
) {
  
  var canvas_element = $("#wordcloud_canvas");
  var canvas_context = canvas_element[0].getContext("2d");
  canvas_context.canvas.width = canvas_element.parent().width();
  canvas_context.canvas.height = canvas_element.parent().height();
  
  let data_list = [];
  
  for (var i in data_json) {
    data_list.push([
      data_json[i]["word"], 
      data_json[i]["size"],
      data_json[i]["color"]
    ]);
  }

  var color_iter = 0;

  WordCloud($("#" + canvas_id)[0], {
    list: data_list,
    gridSize: Math.round(12 * $("#" + canvas_id).width() / 1024),
    weightFactor: scale_size,
    fontFamily: font_family,
    fontWeight: 600,
    color: function () { return data_list[color_iter++][2]; },
    rotateRatio: 0.5,
    rotationSteps: 2,
    minSize: 8,
    shuffle: true
  });
  
}

/*
fetch("assets/data/wordcloud2_data.json")
  .then((response) => response.json())
  .then((json) => { 
    wordcloud_draw("wordcloud_canvas", json, scale_size=3); 
  })
  .catch((error) => console.error('Error fetching data:', error));
*/