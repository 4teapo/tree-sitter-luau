#!lune run

-- Constants.
do
	local CONSTANT = 12
	local non_constant = 4
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
beebo()

type function fun(t: any)
	assert(false, "ok")
end

@native @[fun { hi = "there" }] local function b(c: | d | e, ...): (&f, ...g)
	error("grr", 3)
end

foo.bar:baz()

@native function foo.bar.baz.qux()
end

@native function foo.bar.baz:qux()
end

foo.bar.baz()

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
