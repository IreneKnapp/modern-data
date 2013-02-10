(function() {

var content = null;
var node = null;
var templates = {
"loading": null,
};

var DOM = {
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

var Template = function(text, options) {
    var _options = options || {};
    var _items = [];
    
    while(text.length > 0) {
        var matches;
        
        matches = text.match(/^([^<{])+/);
        if(matches) {
            text = text.substr(matches[0].length);
            var match = matches[0];
            match = match.match(/^\s*(\S*)\s*$/)[1];
            if(match.length > 0) _items.push(match);
            continue;
        }
        
        matches = text.match
            (/^<\s*((?:([a-zA-Z0-9]+):)?[a-zA-Z0-9]+)\s*(?!\s)/);
        if(matches) {
            text = text.substr(matches[0].length);
            var elementNamespace;
            var elementName;
            if(matches.length == 2) {
                elementNamespace = null;
                elementName = matches[1];
            } else if(matches.length == 3) {
                elementNamespace = matches[1];
                elementNamespace = matches[2];
            }
            
            var attributes = [];
            while(true) {
                matches = text.match
                    (/^((?:([a-zA-Z0-9]+):)?[a-zA-Z0-9]+)\s*(?!\s)/);
                if(!matches) break;
                text = text.substr(matches[0].length);
                var attributeNamespace;
                var attributeName;
                if(matches.length == 2) {
                    attributeNamespace = null;
                    attributeName = matches[1];
                } else if(matches.length == 3) {
                    attributeNamespace = matches[1];
                    attributeName = matches[2];
                }
                
                var valueItems = [];
                matches = text.match(/^'((?:[^'\\]|\\\\|\\')*)'/);
                if(matches) {
                }
                
                attributes.push({
                    namespace: attributeNamespace,
                    name: attributeName,
                    value: valueItems,
                });
            }
            _items.push({
                namespace: elementNamespace,
                name: elementName,
                attributes: attributes,
            });
            break;
        }
        
        break;
    }
    
    console.log(_items);
    
    return {
        render: function(variables, options) {
            var options = Higher.Object.merge(_options, options || {}, {
                into: null,
            });
            
            if(options.into != null) {
                DOM.replaceContents(result);
            }
            
            return text;
        },
    };
};

var updatePage = function() {
    if(content == null) {
        console.log(templates.loading.render());
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
    xhr.open("GET", "content.jsonz");
    xhr.send();
};

window.addEventListener("load", function() {
    for(var name in templates) {
        templates[name] = new Template(DOM.Magic.get("template", name));
    }
    
    updatePage();
    loadContent();
});

})();