#!lune run

-- Constants.
do
	local CONSTANT = 12
	local non_constant = 4
end

-- Real constants.
do
	const CONSTANT = 21
	const non_constant = 4444444
	local const = 12
	print(const)
	const += 1
end

-- String interpolation
do
	local interpolated_string = `AAA!\nBBB!\n{1 + 2} + 3 = {1 + 2 + 3}!\nCCC.`
end

--[[

Block comment!

]]

do
	repeat
		local _ = 4
	until false

	while false do
		local _ = 4
	end
end

--[[

Block comment!

]]

type Ok = {
	read x: number,
}

-- Types.
do
	type fun<T...> = (string, T...) -> ()

	export type Ty = {
		read [&string&number]: number,
		read x: string,
		read y: "ok",
		write z: fun<number>,
	}

	export type Un =
		| "a"
		| "b"
		| "c"
end

print(foo.bar.baz.qux)

--[[

Block comment!

]]

local function beebo<A, B, C>() end
local spiderBeebo = beebo<<string, string, string>>
spiderBeebo()
beebo<<number, number, number>>()
beebo<<(number, string), buffer, (foo, bar, baz), qux>>()
beebo()

type function fun(t: any)
	assert(false, "ok")
end

@native @[fun { hi = "there" }] local function b(c: | d | e, ...): (&f, ...g)
	error("grr", 3)
end

const function L(c: | d | e, ...): (g...)
	error("brr", 3)
end

foo.bar:baz()

@native function foo.bar.baz.qux()
end

@native function foo.bar.baz:qux()
end

foo.bar.baz()

local function makeTyper<T>(): () -> T?
	return function() return nil end
end

local typerMake = {}
typerMake.makeTyper = makeTyper

local numberTyper = makeTyper<<number>>()
local stringTyper = typerMake.makeTyper<<string>>()
local bufferTyper = typerMake:makeTyper<<buffer>>()

type Entity<T = nil> = Jecs.Entity<T>

type Foo<A, B...> = Bar<B...> | Baz<A>

local k = 2
local v = 3
k += v * 2 + 3
local _ = k < 3

-- Types.
do
	type Ty = {
		read [string]: number,
		write a: "b",
		write b: boolean,
		read c: "d",
	}
	type Ty2 = typeof(setmetatable({} :: { foo: "bar" }, {} :: { baz: "qux" }))
	type function foo(ty)
		types.singleton("str")[1] += 1
		local _ = ty:is(types.singleton("str"))
		return error("no")
	end
end

--[[

Block comment!

]]

-- Function attributes.
do
	@a local function a()
	end
	@[b { c = true }] function b()
	end
	@[b "c", d { e = "f" }, g ( { o = "k" }, "i", 12 )] function c()
	end
	table.insert(array, @native function()
	end)
end

--[[

Block comment!

]]

-- Type assertion.
do
	local a = (("str" :: "str") :: any) :: string
end

-- Builtins.
do
	local buf = buffer.create(4)
	local notbuf = notbuffer.create(4)

	print(("str"):match("@[^\n\r]"))
	print(("str"):match "@[^\n\r]")
	string.match("str", "@[^\n\r]")
end

--[[

Block comment!

]]

typeconst<const>=any?
consttype:|const?=type
typetype=typeof(type(type))
constconst:|type=table[--[=[]=]"freeze"--[=[]=]]
constcontinue:|const=type
exporttypeexport=typeof(...::|type)
exportlocal export:|export=const(setmetatable({},{__call=@[
  deprecated { use = '\z
                        Hello, World,',},checked
]
@[native] function(_, _)
  return const(_)
end;}));
constconst:|const=const{
  const =  const { if if if const then const < <|const<|const?>?> --[[hi]] > {} else true then if not continue or continue(continue)then continue(continue) else false else if type then export{} else false then 0x1 else 0x_0b_1_0_i_;};
}.const or const { const ::|const???;};
print<<const>>(const)


@[
  deprecated { use = '\z
                        Hello, World,',},checked
]
@[native]
export function foo<T>(a: string, b: | T): (number, T)
	print("hello, world!")
end
export const bar = 12
export local foo = bar(123)

export local export = export
export = 12
