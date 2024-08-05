declare type NativeSchema = symbol;

/// Stub to not include entirety of "@types/node"
declare namespace NodeJS {
  interface Buffer {}
}

declare namespace Jslt {
  export class Jslt {
    constructor(schema: string);

    transform<T = any, R = any>(value?: T): R;

    transformStr(value: string | ArrayBuffer | NodeJS.Buffer): string;

    transformParse<R = any>(value: string | ArrayBuffer | NodeJS.Buffer): R;

    transformStringify<T = any>(value?: T): string;
  }

  export function compile(schema: string): NativeSchema;

  export function transform<T = any, R = any>(schema: string | NativeSchema, value?: T): R;

  export function transformStr(schema: string | NativeSchema, value: string | ArrayBuffer | NodeJS.Buffer): string;

  export function transformParse<R = any>(schema: string | NativeSchema, value: string | ArrayBuffer | NodeJS.Buffer): R;

  export function transformStringify<T = any>(schema: string | NativeSchema, value?: T): string;
}
