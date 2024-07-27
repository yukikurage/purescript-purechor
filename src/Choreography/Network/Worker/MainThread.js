export const isWorkerAvailable = () => window.Worker !== undefined;

export const newWorker = (workerPath) => () => new Worker(workerPath);

export const postMessage = (worker) => (message) => () => {

  worker.postMessage(message)};

export const terminateWorker = (worker) => () => worker.terminate();

export const onMessage = (worker) => (callback) => () =>  {
  console.log("Registering")

  worker.addEventListener('message', (e) => {
    callback(e.data)()
  });
}
