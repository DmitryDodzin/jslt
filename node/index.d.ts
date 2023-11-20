declare type CompiledSchema = symbol;

declare namespace Jslt {
	export function compile(schema: string): CompiledSchema;

	export function transform<T = any, S = any>(schema: string | CompiledSchema, value?: T): S;

	export function transformStr(schema: string | CompiledSchema, value: string): string;
}

export = Jslt;
