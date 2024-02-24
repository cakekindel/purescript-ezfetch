import { fetch, ProxyAgent } from 'undici'
import { socksDispatcher } from 'fetch-socks'

/** @type {(_: URL) => (_: URL) => (_: string) => (_: Record<string, string>) => (_: null | string | FormData) => () => Promise<import('undici').Response>} */
export const fetchImpl = proxyURL => url => method => headers => body => () => {
  const dispatcher = proxyURL.protocol.startsWith('https')
    ? new ProxyAgent(proxyURL.host)
    : proxyURL.protocol.startsWith('socks')
    ? socksDispatcher({
        type: 5,
        host: proxyURL.hostname,
        port: parseInt(proxyURL.port, 10),
      })
    : (() => {
        throw new Error(`unsupported proxy scheme ${proxyURL.protocol}`)
      })()

  return fetch(url, { dispatcher })
}
