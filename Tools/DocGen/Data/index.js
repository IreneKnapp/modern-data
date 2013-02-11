(function() {

var content = null;
var node = null;
var templates = [
"loading",
];

var DOM = {
    replaceBody: function(html) {
        document.body.innerHTML = html;
    },
    
    Magic: {
        get: function(type, name) {
            var candidates = document.getElementsByTagName("script");
            for(var i = 0; i < candidates.length; i++) {
                var candidate = candidates.item(i);
                
                var foundName = candidate.getAttributeNode("name");
                foundName = foundName ? foundName.value : undefined;
                if(foundName != name) continue;
                
                var foundType = candidate.getAttributeNode("type");
                foundType = foundType ? foundType.value : undefined;
                if(foundType != type) continue;
                
                return candidate.innerHTML;
            }
            return null;
        },
    },
};

var Higher = {
    Object: {
        merge: function() {
            var result = {};
            
            for(var i = 0; i < arguments.length; i++) {
                for(var key in arguments[i]) {
                    if(result[key] === undefined)
                        result[key] = arguments[i][key];
                }
            }
            
            return result;
        },
    },
};

var updatePage = function() {
    if(content == null) {
        DOM.replaceBody(templates.loading({}));
    } else {
        console.log("Hm");
    }
};

var loadContent = function() {
    var xhr = new XMLHttpRequest();
    xhr.onreadystatechange = function(event) {
        var xhr = event.target;
        if(xhr.readyState == 4) {
            content = xhr.responseText;
            content = content ? JSON.parse(content) : null;
            updatePage();
        }
    };
    xhr.open("GET", "content.json");
    xhr.send();
};

window.addEventListener("load", function() {
    var result = {};
    for(var i = 0; i < templates.length; i++) {
        var name = templates[i];
        var source = DOM.Magic.get("template", name);
        console.log(source);
        result[name] = Handlebars.compile(source);
    }
    templates = result;
    
    updatePage();
    loadContent();
});

})();