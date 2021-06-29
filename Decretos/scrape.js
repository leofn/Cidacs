var url ='http://www.planalto.gov.br/ccivil_03/Portaria/quadro_portaria.htm';
var page = new WebPage();
var fs = require('fs');

page.open(url, function (status) {
        just_wait();
});

function just_wait() {
    setTimeout(function() {
               fs.write('dec_federal.html', page.content, 'w');
            phantom.exit();
    }, 2500);
}

