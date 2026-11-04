use arbitrary_int::u31;
use bitbybit::bitfield;
use core::ptr::NonNull;

// native inner type

#[bitfield(u32, default = 42, derive(Default, Debug, From))]
#[derive(PartialEq)] // NOTE works because `raw_value` exists after #[bitfield(...)]
struct TestNative {}

fn test_native_int() {
    let t: TestNative = Default::default();
    assert_eq!(42, t.raw_value);

    let debug = format!("{t:?}");
    assert!(debug.starts_with("TestNative"));

    let t = TestNative::from(42);
    assert_eq!(42, t.raw_value);

    let value = u32::from(t);
    assert_eq!(42, value);

    let t: TestNative = value.into();
    assert_eq!(42, t.raw_value);

    let value: u32 = t.into();
    assert_eq!(42, value);
}

// arbitrary_int inner type

#[bitfield(u31, default = 42, derive(Default, Debug, From))]
struct TestArbitrary {}

fn test_arbitrary_int() {
    const _42: u31 = u31::from_u32(42);

    let t: TestArbitrary = Default::default();
    assert_eq!(_42, t.raw_value());

    let debug = format!("{t:?}");
    assert!(debug.starts_with("TestArbitrary"));

    let t = TestArbitrary::from(_42);
    assert_eq!(_42, t.raw_value());

    let value = u31::from(t);
    assert_eq!(_42, value);

    let t: TestArbitrary = value.into();
    assert_eq!(_42, t.raw_value());

    let value: u31 = t.into();
    assert_eq!(_42, value);
}

// generic conversion to/from raw types

trait NonNullExt<T> {
    unsafe fn read_volatile_le(self) -> T;
    unsafe fn read_volatile_be(self) -> T;
    unsafe fn write_volatile_le(self, value: T);
    unsafe fn write_volatile_be(self, value: T);
}

impl<T> NonNullExt<T> for NonNull<T>
where
    T: From<u32> + Into<u32>, // NOTE works on any #[bitfield(u32, derive(From))]
{
    unsafe fn read_volatile_le(self) -> T {
        assert_eq!(size_of::<u32>(), size_of::<T>());
        unsafe { u32::from_le(self.cast::<u32>().read_volatile()).into() }
    }
    unsafe fn read_volatile_be(self) -> T {
        assert_eq!(size_of::<u32>(), size_of::<T>());
        unsafe { u32::from_be(self.cast::<u32>().read_volatile()).into() }
    }
    unsafe fn write_volatile_le(self, val: T) {
        assert_eq!(size_of::<u32>(), size_of::<T>());
        unsafe { self.cast::<u32>().write_volatile(val.into().to_le()) };
    }
    unsafe fn write_volatile_be(self, val: T) {
        assert_eq!(size_of::<u32>(), size_of::<T>());
        unsafe { self.cast::<u32>().write_volatile(val.into().to_be()) };
    }
}

fn test_generic_conversion() {
    let mut le = TestNative::from(1u32.to_le());
    let mut be = TestNative::from(1u32.to_be());
    assert_ne!(le, be);

    let ptr_le = NonNull::from_mut(&mut le);
    assert_eq!(1u32.to_le(), le.raw_value());
    assert_eq!(1u32, unsafe { ptr_le.read_volatile_le().raw_value() });

    let ptr_be = NonNull::from_mut(&mut be);
    assert_eq!(1u32.to_be(), be.raw_value());
    assert_eq!(1u32, unsafe { ptr_be.read_volatile_be().raw_value() });

    unsafe { ptr_le.write_volatile_le(Default::default()) };
    assert_eq!(42u32.to_le(), le.raw_value());

    unsafe { ptr_be.write_volatile_be(Default::default()) };
    assert_eq!(42u32.to_be(), be.raw_value());
}

fn main() {
    test_native_int();
    test_arbitrary_int();
    test_generic_conversion();
}
