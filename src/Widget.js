"use strict";

exports.createDerivedElement = function (localName) {
    return function(parentName) {
        return function (doc) {
            return function () {
                return doc.createElement(parentName, { is: localName });
            };
        };
    };
};