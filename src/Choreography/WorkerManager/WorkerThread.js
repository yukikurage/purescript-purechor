export const postMessageToMain = (message) => () => postMessage(message);

export const onMessageFromMain = (callback) => () =>
  addEventListener("message", (e) => callback(e.data)());
