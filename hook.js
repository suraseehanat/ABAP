var axios = require('axios');
var express = require('express')
var app = express();
var bodyParser = require('body-parser')
var port = 3000

app.use(bodyParser.json())

var data = 'payload={\n    "text": "Hello World"\n    \n}';

var config1 = {
  method: 'post',
  url: 'https://192.168.21.23:5001/webapi/entry.cgi?api=SYNO.Chat.External&method=incoming&version=2&token=%22aDWKqpE0oWIG4H86xxKG7bNsYZTWv62BSzuSGPfL9yweq1K1Z4bQg61BGhsiU2bZ%22',
  headers: { 
    'Content-Type': 'application/json'
  },
  data : data
};
var config2 = {
    method: 'post',
    url: 'http://43.249.113.70:5000/webapi/entry.cgi?api=SYNO.Chat.External&method=incoming&version=2&token=%224agrbQtPLkaR9aWqzoKvpOvqfDZhDKIr2F9HgkAsAvPD5p3ivuiLY9o5KspSVzlu%22',
    headers: { 
      'Content-Type': 'application/json'
    },
    data : data
  };

  var config3 = {
    method: 'post',
    url: 'http://43.249.113.70:5000/webapi/entry.cgi?api=SYNO.Chat.External&method=incoming&version=2&token=%22pDOuIndrQPlQmRpCKq6y09FioRH4zJDjGZibyfHdRULtDZj1bT9rrF5IPNDgOstN%22',
    headers: { 
      'Content-Type': 'application/json'
    },
    data : data
  };

  var config4 = {
    method: 'post',
    url: 'http://43.249.113.70:5000/webapi/entry.cgi?api=SYNO.Chat.External&method=incoming&version=2&token=%22USpyNs9J7WG2oWyDivnpCywmxPslYLiwyAoDBnm8m6kVYG73UhGfmzw3dyR7Cs8w%22',
    headers: { 
      'Content-Type': 'application/json'
    },
    data : data
  };
  function timeout(ms) {
    return new Promise(resolve => setTimeout(resolve, ms));
}
  async function url1() {
    
  
    await axios(config1)
    .then(function (response) {
      //console.log(JSON.stringify(response.data));
      var code = JSON.stringify(response.data);
    
      console.log ("url1");
      console.log(response.data.success);
      return response.data.success
    })
    .catch(function (error) {
      console.log(error);
    });
    await timeout(5000);
  }

  async function url2() {
    
    await timeout(5000);
    await axios(config2)
    .then(function (response) {
      //console.log(JSON.stringify(response.data));
      var code = JSON.stringify(response.data);
    
      console.log ("url2");
      console.log(response.data.success);
    })
    .catch(function (error) {
      console.log(error);
    });
    
  }
async function url3() {
    
    //await sleep(2000); 
    await timeout(10000);
    await axios(config3)
    .then(function (response) {
      //console.log(JSON.stringify(response.data));
      var code = JSON.stringify(response.data);
    
      console.log ("url3");
      console.log(response.data.success);
    })
    .catch(function (error) {
      console.log(error);
    });
    
  }
  async function url4() {
    
    //await sleep(2000); 
    await timeout(15000);
    await axios(config4)
    .then(function (response) {
      //console.log(JSON.stringify(response.data));
      var code = JSON.stringify(response.data);
    
      console.log ("url4");
      console.log(response.data.success);
    })
    .catch(function (error) {
      console.log(error);
    });
    
  }

  app.get('/', (req, res) => {
    res.send('Hello World!')
  })

  app.post('/webhook', async (req, res) => {
    const payload = await req.body;
    console.log(payload);
    var returnURL1 = url1();
    var returnURL2 = url2();
    var returnURL3 = url3();
    var returnURL4 = url4();
    if (returnURL1 == "false"){
      url1();
    }
    if (returnURL2 == "false"){
      url2();
    }
    if (returnURL3 == "false"){
      url3();
    }
    if (returnURL4 == "false"){
      url4();
    }
    
    res.json(payload);
    });

  app.listen(port, () => {
    console.log(`Example app listening at http://localhost:${port}`)
  })
