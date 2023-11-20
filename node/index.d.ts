declare type CompiledSchema = symbol;

declare namespace Jslt {
	export function compile(schema: string): CompiledSchema;

	export function transform<T = any, R = any>(schema: string | CompiledSchema, value?: T): R;

	export function transformStr(schema: string | CompiledSchema, value: string): string;

	export function transformParse<R = any>(schema: string | CompiledSchema, value: string): R;
}

export = Jslt;
