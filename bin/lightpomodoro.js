#! env node
const http = require('http');
const fs = require('fs');

const totalSeconds = (process.argv[2] || 25) * 60;
const token = process.env.HASS_TOKEN;
let isOn = true;
let leftSeconds = totalSeconds;
let interval = 2 * 1000;

var options = {
  'method': 'POST',
  'hostname': '192.168.31.20',
  'port': 8123,
  'path': '/api/services/light/turn_on',
  'headers': {
    'Content-Type': 'application/json',
    'Authorization': `Bearer ${token}`
  }
};

const send = (postData, callback) => {

  var req = http.request(options, function (res) {
    var chunks = [];

    res.on("data", function (chunk) {
      chunks.push(chunk);
    });

    res.on("end", function (chunk) {
      var body = Buffer.concat(chunks);
      // console.log(body.toString());
      if (callback) callback();
    });

    res.on("error", function (error) {
      console.error(error);
      if (callback) callback();
    });
  });


  req.write(JSON.stringify(postData));
  req.end();
}

const lerp = (endRGB, startRGB, percent) => {
  const p = percent / 100;
  const color = startRGB.map((color, index) => Math.floor((1 - p) * color + p* endRGB[index]))
  return color;
};

setInterval(() => {
  leftSeconds = leftSeconds - 1;
}, 1 * 1000);

setColors = () => {
  const percent = leftSeconds * 100 / totalSeconds;

  let postData = {
    "entity_id": "light.small_neolight",
  };

  const startRGB = [255, 240, 124];
  const endRGB = [255, 0, 0];

  if (percent > 10) {
    postData = {
      ...postData,
      "rgb_color": lerp(startRGB, endRGB, percent)
    };
  } else {
    postData = {
      ...postData,
      "rgb_color": [255, 0, 0],
      "brightness": isOn ? 0 : 255,
    };
    isOn = !isOn;
    interval = 500;
  }

  send(postData);

  setTimeout(setColors, interval);

  if (leftSeconds < 1) {
    // process.exit();
  }
};

setTimeout(setColors , interval);

const clear = () => {
  const postData = {
    "entity_id": "light.small_neolight",
    "brightness": 0,
  };
  send(postData, process.exit);
}

process.on('exit', clear);
process.on('SIGINT',  clear);
process.on('SIGUSR1', clear);
process.on('SIGUSR2', clear);
process.on('uncaughtException', clear);
