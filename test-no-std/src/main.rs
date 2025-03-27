#![no_std]
#![no_main]

#[bitbybit::bitfield(u32, debug)]
pub struct TestReg {
    #[bits(0..=31, rw)]
    value: u32,
}

#[panic_handler]
fn panic(_info: &core::panic::PanicInfo) -> ! {
    loop {}
}
