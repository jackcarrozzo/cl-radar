// from https://github.com/processing/p5.js-sound/pull/322

function setup() {
  var ac = getAudioContext();

  ac.suspend().then(function() {
    var myButton = createButton('click to start audio');
    myButton.position(0, 0);

    userStartAudio(myButton, function() {
      alert(ac.state);
      myButton.remove();
    });
  });
}
