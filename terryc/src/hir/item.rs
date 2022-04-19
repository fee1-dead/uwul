use super::HirId;

pub enum Item {
    Fn(ItemFn),
}

pub struct ItemFn {
    id: HirId,
}