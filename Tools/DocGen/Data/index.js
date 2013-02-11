(function() {

var content = null;
var node = null;
var templates = [
"loading",
"page",
"textItem",
"bodyItem",
];

var Is = {
    string: function(object) {
        return typeof object == "string";
    },
    
    array: function(object) {
        return Object.prototype.toString.call(object) == '[object Array]';
    },
};

var DOM = {
    setTitle: function(title) {
        document.title = title;
    },
    
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

var handleNavigation = function(navigationIn) {
    if(Is.array(navigationIn)) {
        var navigationOut = [];
        for(var i = 0; i < navigationIn.length; i++) {
            navigationOut.push(handleTextItems(navigationIn[i]));
        }
        return navigationOut;
    } else {
        console.log(navigationIn);
    }
};

var handleBody = function(bodyIn) {
    if(Is.array(bodyIn)) {
        var bodyOut = [];
        for(var i = 0; i < bodyIn.length; i++) {
            bodyOut.push(handleBodyItem(bodyIn[i]));
        }
        return bodyOut;
    } else {
        console.log(bodyIn);
    }
};

var handleBodyItem = function(bodyItemIn) {
    if(Is.array(bodyItemIn)) {
        if(bodyItemIn[0] == "header") {
            return {
                header: true,
                contents: handleTextItems(bodyItemIn.slice(1)),
            };
        } else if(bodyItemIn[0] == "paragraph") {
            return {
                paragraph: true,
                contents: handleTextItems(bodyItemIn.slice(1)),
            };
        } else {
            console.log(bodyItemIn);
        }
    } else {
        console.log(bodyItemIn);
    }
};

var handleTextItems = function(textItemsIn) {
    if(Is.array(textItemsIn)) {
        var textItemsOut = [];
        for(var i = 0; i < textItemsIn.length; i++) {
            textItemsOut.push(handleTextItem(textItemsIn[i]));
        }
        return textItemsOut;
    } else {
        console.log(textItemsIn);
    }
};

var handleTextItem = function(textItemIn) {
    if(Is.string(textItemIn)) {
        return {
            string: true,
            value: textItemIn,
        };
    } else if(Is.array(textItemIn)) {
        if(textItemIn[0] == 'link') {
            return {
                link: true,
                href: textItemIn[1],
                contents: handleTextItems(textItemIn.slice(2)),
            };
        } else {
            console.log(textItemIn);
        }
    } else {
        console.log(textItemIn);
    }
};

var updatePage = function() {
    if(content == null) {
        DOM.replaceBody(templates.loading({}));
    } else {
        var sectionID = content.toc;
        var section = content.sections[sectionID];
        DOM.setTitle(section.title);
        DOM.replaceBody(templates.page({
            navigation: handleNavigation(section.navigation),
            body: handleBody(section.body),
        }));
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
    for(var i = 0; i < templates.length; i++) {
        (function() {
            var name = templates[i];
            Handlebars.registerHelper(name, function(context) {
                console.log(name, context);
                return new Handlebars.SafeString(templates[name](context));
            });
        })();
    }
    
    var result = {};
    for(var i = 0; i < templates.length; i++) {
        var name = templates[i];
        var source = DOM.Magic.get("template", name);
        result[name] = Handlebars.compile(source);
    }
    templates = result;
    
    updatePage();
    loadContent();
});

})();
