/** @type {(_: Record<string, Array<string | Blob>>) => () => FormData} */
export const unsafeMakeFormData = o => () => {
  const form = new FormData()

  Object.entries(o).forEach(([k, vs]) => {
    vs.forEach(v => {
      form.append(k, v)
    })
  })

  return form
}

/** @typedef {{filename: string | null, mime: string, buf: ArrayBuffer}} FileRecord */

/** @type {(_: FileRecord) => () => Blob} */
export const unsafeMakeBlob =
  ({ mime, buf }) =>
  () =>
    new Blob([buf], { type: mime })

/** @type {(_: FormData) => () => Promise<Record<string, Array<string | FileRecord>>>} */
export const unsafeUnmakeFormData = fd => async () => {
  /** @type {Record<string, Array<string | FileRecord>>} */
  const rec = {}
  for (const [k, ent_] of fd.entries()) {
    /** @type {File | Blob | string} */
    const ent = ent_

    /** @type {string | FileRecord} */
    let append = ''
    if (ent instanceof File) {
      append = {
        filename: ent.name,
        buf: await ent.arrayBuffer(),
        mime: ent.type,
      }
    } else if (ent instanceof Blob) {
      append = { filename: null, buf: await ent.arrayBuffer(), mime: ent.type }
    } else {
      append = ent
    }

    if (!append) {
      continue
    }

    if (rec[k]) {
      rec[k].push(append)
    } else {
      rec[k] = [append]
    }
  }

  return rec
}
