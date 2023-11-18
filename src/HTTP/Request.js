/** @type {(_: Blob) => () => Promise<ArrayBuffer>} */
export const blobArrayBufferImpl = b => () => b.arrayBuffer()
