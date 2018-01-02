# Duolingo Simplified to Traditional Chinese

![Duolingo with Traditional Characters](https://i.imgur.com/YL8yQUI.jpg)


This extension allows users to read Chinese as it written in Taiwan and Hong Kong whilst learning Chinese in Duolingo. From an an etymological standpoint, many simplified characters no longer make sense and have crucial strokes removed from them.

If you're interested in reading any Chinese literature published before the 1950s it will be most advantageous to learn the traditional characters. Also, it is easier to read simplified if one has learned traditional Chinese, than the reverse.

The extension is installable at https://chrome.google.com/webstore/detail/clnnjdojceobnkhkocigpboialomopfk/


### Installing for development

First you'll need to pull down the development version

```
git clone https://github.com/robincard/duolingo-simplified-to-traditional-chinese.git
```

then cd into the directory

```
cd duolingo-simplified-to-traditional-chinese
```

After that you'll need to use npm to get all of the dependencies. If you don't have npm or node, you'll need to [download Node.js and NPM](https://nodejs.org/en/)

```
npm install
```

Then in Chrome to go manage extensions click **enable developer node** then **Load unpacked extension** then locate the `dist` directory of this project

![https://i.imgur.com/cvFgeFX.png](https://i.imgur.com/cvFgeFX.png)

After that run npm start, any changes you make will automatically be saved to your dist directory, but you'll need to refresh the extension for Chrome to load the changes.

```
npm start
```

## Built With

* [Webpack](https://webpack.js.org/) - The world's greatest asset bundler
* [Babel](https://babeljs.io/) - Has enabled the project to be built with ES6 and beyond
* [Sizzle](https://sizzlejs.com/) - A stripped down, minimal CSS selector for Javascript
* [uglifyjs](https://github.com/mishoo/UglifyJS) - A great minifier for JavaScript
* [Node](https://nodejs.org/en/) - NodeJS

## Browser dependencies and MutationObserver
This extension relies on a [DOM Level 4](https://www.w3.org/TR/dom/) addition to the Node functionaity of Javascript called [MutationObserver](https://www.w3.org/TR/dom/#interface-mutationobserver). Officially DOM 4 is still a way from being finished but MutationObservers have been implemented by every major browser.

This project wouldn't have been possible without MutationObserver. MutationObserver allows you to observe for changes within a chosen section of your DOM tree, this was possible with MutationEvents which were introduced around 10 years ago but they slowed down browser performance by up to 700% as well as support being extremely fragmented.
