var url ='http://www.edreams.es/#/results/type=R;dep=2017-07-03;from=BCN;to=BRQ;ret=2017-07-15;collectionmethod=false;airlinescodes=false;internalSearch=true';
var page = new WebPage()
var fs = require('fs');


page.open(url, function (status) {
        just_wait();
});

function just_wait() {
    setTimeout(function() {
               fs.write('1.html', page.content, 'w');
            phantom.exit();
    }, 40000);
}
