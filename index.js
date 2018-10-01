
if (module.hot) {
  module.hot.accept(function () {
    window.location.reload();
  });
}

require("./output/Main").main();
