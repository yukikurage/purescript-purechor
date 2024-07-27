
export const postMessageToMain = (message) => () => postMessage(message);

export const onMessage = (callback) => () => onmessage = (e) => callback(e.data)();
