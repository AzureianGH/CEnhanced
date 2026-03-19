# CHance H28 Draft Overlay

H27 is the baseline. H28 adds bundles (class model) and a few upgrades.

## 1. Scope

H28 adds:

- bundles (`bundle`)
- constructors/destructors (`+this`, `-this`)
- visibility (`public`, `private`, `internal`)
- static members
- contracts (interface-like types)
- better sum/error modeling (`Result`/`Option` direction)

H28 does not add:

- GC requirement
- multiple inheritance
- hidden allocations in normal method calls

## 2. Language Mode

- `-H28`: enable H28 features
- `-H27`: unchanged

## 3. Bundles

### 3.1 Declaration

```chance
bundle Window {
    i32 x;
    i32 y;

    fun Move(i32 nx, i32 ny) -> void {
        this.x = nx;
        this.y = ny;
    }
}
```

### 3.2 Rules

- bundle is nominal
- methods get implicit `this`
- field layout is deterministic
- values can be stack or heap allocated

### 3.3 Lifecycle

```chance
bundle Vec2 {
    i32 x;
    i32 y;

    +this(i32 ax, i32 ay) {
        this.x = ax;
        this.y = ay;
    }

    -this() {
    }
}

Vec2 v = Vec2(10, 20);
Vec2* p = new Vec2(1, 2);
delete p;
```

- stack values run `-this` at scope exit
- heap values run `-this` before delete release

### 3.4 Access and Static

- `public`, `private`, `internal`
- `static` fields and methods supported

### 3.5 Inheritance/Dispatch

- default direction: composition + contracts
- optional later: single inheritance
- no multiple inheritance
- dynamic dispatch only through contract-typed references

## 4. Contracts

```chance
contract Drawable {
    fun Draw() -> void;
}
```

Bundles can implement contracts for explicit polymorphism.

## 5. ABI Requirements

H28 should pin:

- field order/alignment
- symbol mangling for bundle methods
- +this/-this symbol convention
- vtable layout if dynamic dispatch is enabled

## 6. Useful Additions in H28

- sum types with exhaustive `match`
- `Result<T, E>` and `Option<T>` standardization
- stronger generic constraint diagnostics
- better constructors/method resolution errors

## 7. Migration from H27

- existing `struct`/`object` code remains valid
- bundles are additive
- keep `struct` for plain-data/interop
- use bundles for behavior + lifecycle types

## 8. Rollout Plan

1. `bundle` parser/sema + `this`
2. `+this`/`-this` + visibility + static members
3. contracts + explicit dispatch path
4. sum/error model additions

## 9. Test Plan

- parser tests: `bundle`, `+this`, `-this`, visibility, contracts
- sema tests: access checks, method lookup, dispatch rules
- codegen tests: +this/-this/static members
- ABI snapshot tests for symbols/layout
- mixed mode tests for `-H27` and `-H28`
