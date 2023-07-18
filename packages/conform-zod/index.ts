import {
	type FieldConstraint,
	type FieldsetConstraint,
	type Submission,
	getName,
	parse as baseParse,
	VALIDATION_SKIPPED,
	VALIDATION_UNDEFINED,
} from '@conform-to/dom';
import * as z from 'zod';

export function coerceString(
	value: unknown,
	transform?: (text: string) => unknown,
) {
	if (typeof value !== 'string') {
		return value;
	}

	if (value === '') {
		return undefined;
	}

	if (typeof transform !== 'function') {
		return value;
	}

	return transform(value);
}

export function coerceFile(file: unknown) {
	if (file instanceof File && file.name === '' && file.size === 0) {
		return undefined;
	}

	return file;
}

export function enhanceSchema<Type>(schema: z.ZodType<Type>): z.ZodType<Type> {
	/**
	 * We might be able to fix all type errors with function overloads
	 * But I'm not sure if it's worth the effort
	 */
	if (
		schema instanceof z.ZodString ||
		schema instanceof z.ZodEnum ||
		schema instanceof z.ZodLiteral
	) {
		// @ts-expect-error see message above
		return z.preprocess((value) => coerceString(value), schema);
	} else if (schema instanceof z.ZodNumber) {
		// @ts-expect-error see message above
		return z.preprocess((value) => coerceString(value, Number), schema);
	} else if (schema instanceof z.ZodBoolean) {
		// @ts-expect-error see message above
		return z.preprocess((value) => coerceString(value, Boolean), schema);
	} else if (schema instanceof z.ZodDate) {
		// @ts-expect-error see message above
		return z.preprocess(
			(value) => coerceString(value, (timestamp) => new Date(timestamp)),
			schema,
		);
	} else if (schema instanceof z.ZodArray) {
		// @ts-expect-error see message above
		return z.preprocess(
			(value) => {
				// No preprocess needed if the value is already an array
				if (Array.isArray(value)) {
					return value;
				}

				if (
					typeof coerceString(value) === 'undefined' ||
					typeof coerceFile(value) === 'undefined'
				) {
					return [];
				}

				// Wrap it in an array if the file is valid
				return [value];
			},
			new z.ZodArray({
				...schema._def,
				type: enhanceSchema(schema.element),
			}),
		);
	} else if (schema instanceof z.ZodObject) {
		// @ts-expect-error see message above
		return z.object(
			Object.fromEntries(
				Object.entries(schema.shape).map(([key, def]) => [
					key,
					// @ts-expect-error see message above
					enhanceSchema(def),
				]),
			),
		);
	} else if (schema instanceof z.ZodIntersection) {
		// @ts-expect-error see message above
		return new z.ZodIntersection({
			...schema._def,
			left: enhanceSchema(schema._def.left),
			right: enhanceSchema(schema._def.right),
		});
	} else if (schema instanceof z.ZodUnion) {
		return new z.ZodUnion({
			...schema._def,
			options: schema.options.map(enhanceSchema),
		});
	} else if (schema instanceof z.ZodDiscriminatedUnion) {
		return new z.ZodDiscriminatedUnion({
			...schema._def,
			options: schema.options.map(enhanceSchema),
		});
	} else if (schema instanceof z.ZodTuple) {
		// @ts-expect-error see message above
		return new z.ZodTuple({
			...schema._def,
			items: schema.items.map(enhanceSchema),
		});
	} else if (schema instanceof z.ZodPipeline) {
		// @ts-expect-error see message above
		return new z.ZodPipeline({
			...schema._def,
			in: enhanceSchema(schema._def.in),
		});
	} else if (schema instanceof z.ZodEffects) {
		// A file schema is usually defined as `z.instanceOf(File)`
		// which is implemented based on ZodAny with `superRefine`
		// You can check the `instanceOfType` function on zod for more info
		if (
			schema._def.effect.type === 'refinement' &&
			schema.innerType() instanceof z.ZodAny
		) {
			// @ts-expect-error see message above
			return z.preprocess((value) => coerceFile(value), schema);
		}

		return new z.ZodEffects({
			...schema._def,
			schema: enhanceSchema(schema.innerType()),
		});
	} else if (schema instanceof z.ZodOptional) {
		// @ts-expect-error see message above
		return new z.ZodOptional({
			...schema._def,
			innerType: enhanceSchema(schema.unwrap()),
		});
	} else if (schema instanceof z.ZodDefault) {
		// @ts-expect-error see message above
		return new z.ZodDefault({
			...schema._def,
			innerType: enhanceSchema(schema.removeDefault()),
		});
	}

	return schema;
}

export function getFieldsetConstraint<Source extends z.ZodTypeAny>(
	source: Source,
): FieldsetConstraint<z.input<Source>> {
	function inferConstraint<T>(schema: z.ZodType<T>): FieldConstraint<T> {
		let constraint: FieldConstraint = {};

		if (schema instanceof z.ZodEffects) {
			constraint = {
				...inferConstraint(schema.innerType()),
			};
		} else if (schema instanceof z.ZodPipeline) {
			constraint = {
				...inferConstraint(schema._def.out),
			};
		} else if (schema instanceof z.ZodOptional) {
			constraint = {
				...inferConstraint(schema.unwrap()),
				required: false,
			};
		} else if (schema instanceof z.ZodDefault) {
			constraint = {
				...inferConstraint(schema.removeDefault()),
				required: false,
			};
		} else if (schema instanceof z.ZodArray) {
			constraint = {
				...inferConstraint(schema.element),
				multiple: true,
			};
		} else if (schema instanceof z.ZodString) {
			for (let check of schema._def.checks) {
				switch (check.kind) {
					case 'min':
						if (!constraint.minLength || constraint.minLength < check.value) {
							constraint.minLength = check.value;
						}
						break;
					case 'max':
						if (!constraint.maxLength || constraint.maxLength > check.value) {
							constraint.maxLength = check.value;
						}
						break;
					case 'regex':
						if (!constraint.pattern) {
							constraint.pattern = check.regex.source;
						}
						break;
				}
			}
		} else if (schema instanceof z.ZodNumber) {
			for (let check of schema._def.checks) {
				switch (check.kind) {
					case 'min':
						if (!constraint.min || constraint.min < check.value) {
							constraint.min = check.value;
						}
						break;
					case 'max':
						if (!constraint.max || constraint.max > check.value) {
							constraint.max = check.value;
						}
						break;
				}
			}
		} else if (schema instanceof z.ZodEnum) {
			constraint.pattern = schema.options
				.map((option: string) =>
					// To escape unsafe characters on regex
					option.replace(/[|\\{}()[\]^$+*?.]/g, '\\$&').replace(/-/g, '\\x2d'),
				)
				.join('|');
		}

		if (typeof constraint.required === 'undefined') {
			constraint.required = true;
		}

		return constraint;
	}

	const keys: Array<keyof FieldConstraint> = [
		'required',
		'minLength',
		'maxLength',
		'min',
		'max',
		'step',
		'multiple',
		'pattern',
	];

	function resolveFieldsetConstraint<T extends Record<string, any>>(
		schema: z.ZodType<T>,
	): FieldsetConstraint<z.input<Source>> {
		if (schema instanceof z.ZodObject) {
			const result: FieldsetConstraint<z.input<Source>> = {};

			for (const [key, def] of Object.entries(schema.shape)) {
				// @ts-expect-error
				result[key] = inferConstraint(def);
			}

			return result;
		}

		if (schema instanceof z.ZodEffects) {
			return resolveFieldsetConstraint(schema.innerType());
		} else if (schema instanceof z.ZodOptional) {
			return resolveFieldsetConstraint(schema.unwrap());
		} else if (schema instanceof z.ZodIntersection) {
			return {
				...resolveFieldsetConstraint(schema._def.left),
				...resolveFieldsetConstraint(schema._def.right),
			};
		} else if (
			schema instanceof z.ZodUnion ||
			schema instanceof z.ZodDiscriminatedUnion
		) {
			const options = schema.options as Array<z.ZodType<any>>;

			return options.map(resolveFieldsetConstraint).reduce((prev, next) => {
				const list = new Set([...Object.keys(prev), ...Object.keys(next)]);
				const result: Record<string, FieldConstraint> = {};

				for (const name of list) {
					// @ts-expect-error
					const prevConstraint = prev[name];
					// @ts-expect-error
					const nextConstraint = next[name];

					if (prevConstraint && nextConstraint) {
						result[name] = {};

						for (const key of keys) {
							if (
								typeof prevConstraint[key] !== 'undefined' &&
								typeof nextConstraint[key] !== 'undefined' &&
								prevConstraint[key] === nextConstraint[key]
							) {
								// @ts-expect-error
								result[name][key] = prevConstraint[key];
							}
						}
					} else {
						result[name] = {
							...prevConstraint,
							...nextConstraint,
							required: false,
						};
					}
				}

				return result;
			});
		}

		return {};
	}

	return resolveFieldsetConstraint(source);
}

export function parse<Schema extends z.ZodTypeAny>(
	payload: FormData | URLSearchParams,
	config: {
		schema: Schema | ((intent: string) => Schema);
		async?: false;
		errorMap?: z.ZodErrorMap;
		stripEmptyValue?: boolean;
	},
): Submission<z.output<Schema>>;
export function parse<Schema extends z.ZodTypeAny>(
	payload: FormData | URLSearchParams,
	config: {
		schema: Schema | ((intent: string) => Schema);
		async: true;
		errorMap?: z.ZodErrorMap;
		stripEmptyValue?: boolean;
	},
): Promise<Submission<z.output<Schema>>>;
export function parse<Schema extends z.ZodTypeAny>(
	payload: FormData | URLSearchParams,
	config: {
		schema: Schema | ((intent: string) => Schema);
		async?: boolean;
		errorMap?: z.ZodErrorMap;
		stripEmptyValue?: boolean;
	},
): Submission<z.output<Schema>> | Promise<Submission<z.output<Schema>>> {
	return baseParse<z.output<Schema>>(payload, {
		resolve(payload, intent) {
			let schema: z.ZodType<Schema> =
				typeof config.schema === 'function'
					? config.schema(intent)
					: config.schema;

			if (config.stripEmptyValue ?? true) {
				schema = enhanceSchema(schema);
			}

			const resolveResult = (
				result: z.SafeParseReturnType<z.input<Schema>, z.output<Schema>>,
			): { value: z.output<Schema> } | { error: Record<string, string[]> } => {
				if (result.success) {
					return {
						value: result.data,
					};
				}

				return {
					error: result.error.errors.reduce<Record<string, string[]>>(
						(result, e) => {
							const name = getName(e.path);

							result[name] = [...(result[name] ?? []), e.message];

							return result;
						},
						{},
					),
				};
			};

			return config.async
				? schema
						.safeParseAsync(payload, { errorMap: config.errorMap })
						.then(resolveResult)
				: resolveResult(
						schema.safeParse(payload, { errorMap: config.errorMap }),
				  );
		},
	});
}

/**
 * A helper function to define a custom constraint on a superRefine check.
 * Mainly used for async validation.
 *
 * @see https://conform.guide/api/zod#refine
 */
export function refine(
	ctx: z.RefinementCtx,
	options: {
		/**
		 * A validate function. If the function returns `undefined`,
		 * it will fallback to server validation.
		 */
		validate: () => boolean | Promise<boolean> | undefined;
		/**
		 * Define when the validation should be run. If the value is `false`,
		 * the validation will be skipped.
		 */
		when?: boolean;
		/**
		 * The message displayed when the validation fails.
		 */
		message: string;
		/**
		 * The path set to the zod issue.
		 */
		path?: z.IssueData['path'];
	},
): void | Promise<void> {
	if (typeof options.when !== 'undefined' && !options.when) {
		ctx.addIssue({
			code: z.ZodIssueCode.custom,
			message: VALIDATION_SKIPPED,
			path: options.path,
		});
		return;
	}

	// Run the validation
	const result = options.validate();

	if (typeof result === 'undefined') {
		// Validate only if the constraint is defined
		ctx.addIssue({
			code: z.ZodIssueCode.custom,
			message: VALIDATION_UNDEFINED,
			path: options.path,
		});
		return;
	}

	const reportInvalid = (valid: boolean) => {
		if (valid) {
			return;
		}

		ctx.addIssue({
			code: z.ZodIssueCode.custom,
			message: options.message,
			path: options.path,
		});
	};

	return typeof result === 'boolean'
		? reportInvalid(result)
		: result.then(reportInvalid);
}

export function ifNonEmptyString(
	fn: (value: string) => unknown,
): (value: unknown) => unknown {
	return (value: unknown) => {
		if (typeof value !== 'string') {
			return value;
		}

		if (value === '') {
			return undefined;
		}

		return fn(value);
	};
}
