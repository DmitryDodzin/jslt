declare type NativeSchema = symbol;

declare namespace Jslt {
  export class Jslt {
    constructor(schema: string);

    transform<T = any, R = any>(value?: T): R;

    transformStr(value: string): string;

    transformParse<R = any>(value: string): R;

    transformStringify<T = any>(value?: T): string;
  }

  export function compile(schema: string): NativeSchema;

  export function transform<T = any, R = any>(schema: string | NativeSchema, value?: T): R;

  export function transformStr(schema: string | NativeSchema, value: string): string;

  export function transformParse<R = any>(schema: string | NativeSchema, value: string): R;

  export function transformStringify<T = any>(schema: string | NativeSchema, value?: T): string;
}

export = Jslt;
