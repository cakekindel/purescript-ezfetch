/** @type {(_: string) => (_: string) => (_: Record<string, string>) => (_: null | string | FormData) => () => Promise<Response>} */
export const fetchImpl = url => method => headers => body => () =>
  fetch(url, { redirect: 'manual', body, method, headers })
