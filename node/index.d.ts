declare type NativeSchema = symbol;

declare namespace Jslt {
	export function compile(schema: string): NativeSchema;

	export function transform<T = any, R = any>(schema: string | NativeSchema, value?: T): R;

	export function transformStr(schema: string | NativeSchema, value: string): string;

	export function transformParse<R = any>(schema: string | NativeSchema, value: string): R;
}

export = Jslt;
