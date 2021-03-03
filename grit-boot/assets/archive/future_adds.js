function selectedY() {
makeplot()
}

function makeplot() {
Plotly.d3.csv('assets/data/jira_dump_260221.tsv.sorted.csv',
function(data){ processData(data) });
}

var layout = {
autosize: true,
width: 800,
height: 800,
xaxis:
{title: {text: "TolID"}},
yaxis:
{title: {text: "Manual Interventions of Genome"}},
showlegend: true
}

function processData(allRows) {
console.table(allRows);
var x = [], y = [], colour = [];
var xaxis = $("#dataBtn1").val()
var yaxis = $("#dataBtn2").val()
var colaxis = $("#dataBtn3").val()

for (var i=0; i<allRows.length; i++) {
row = allRows[i];
x.push( row[xaxis] );
y.push( row[yaxis] );
colour.push( getColour(row[colaxis]) );
}
makePlotly( x, y, colour, layout );
}

function rand_colour () {
var r = Math.random()*255;
var g = Math.random()*255;
var b = Math.random()*255;
return 'rgb('+r+','+g+','+b+')'
}

var colour_map = {}

function getColour(project) {
if (project in colour_map) {return colour_map[project]}
colour_map[project] = rand_colour();
return colour_map[project]
}

function makePlotly( x, y, colour, layout){
var traces = [{
x: x,
y: y,
type: "scatter",
mode: "markers",
marker: {
color: colour,
width: 2
};
}];
var config = {responsive: true, displayModeBar: true}
Plotly.react('chart', traces, layout, config);
}

makeplot();