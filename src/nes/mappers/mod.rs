use crate::{
    Access, CurrentMirroring, MutAccess, Rom,
    mappers::{mmc1::Mmc1, nrom::Nrom},
};
use dyn_clone::DynClone;
use log::info;

mod mmc1;
mod nrom;

pub trait Mapper: Access + MutAccess + DynClone {
    fn name(&self) -> &'static str;

    fn step(&mut self) {}
}

dyn_clone::clone_trait_object!(Mapper);

pub fn load(rom: Rom) -> (Box<dyn Mapper>, CurrentMirroring) {
    let mirroring = CurrentMirroring::new(rom.mirroring.into());

    let mapper_id = rom.mapper_id;

    let mapper: Box<dyn Mapper> = match rom.mapper_id {
        0 => Box::new(Nrom::new(rom)),
        1 => Box::new(Mmc1::new(rom, mirroring.clone())),
        _ => unimplemented!("mapper id {}", rom.mapper_id),
    };

    info!("Mapper ID: {:03} ({})", mapper_id, mapper.name());
    info!("Initial mirroring: {:?}", mirroring.get());

    (mapper, mirroring)
}
