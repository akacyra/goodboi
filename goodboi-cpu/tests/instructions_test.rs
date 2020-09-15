use serde::Deserialize;
use std::collections::{BTreeMap, HashMap};

use goodboi_cpu::instructions;

const INSTRUCTION_TABLES_JSON: &str = std::include_str!("data/instructions.json");

#[derive(Deserialize, Clone)]
struct InstructionData {
    name: String,
    bytes: u32,
    cycles: u32,
    cycles_taken: Option<u32>,
}

fn get_unprefixed_instruction_table() -> BTreeMap<u8, InstructionData> {
    let tables: HashMap<String, BTreeMap<String, InstructionData>> =
        serde_json::from_str(INSTRUCTION_TABLES_JSON).unwrap();
    tables
        .get("unprefixed")
        .unwrap()
        .iter()
        .map(|(opcode, data)| (u8::from_str_radix(opcode, 16).unwrap(), data.clone()))
        .collect()
}

#[test]
fn all_unprefixed_instructions_are_decoded_correctly() {
    let decoded: BTreeMap<_, _> = (0..=255)
        .map(|opcode| {
            let mut bytes = vec![opcode, 0x34, 0x12].into_iter();
            (opcode, instructions::decode(&mut bytes).unwrap())
        })
        .collect();

    let table = get_unprefixed_instruction_table();

    for (opcode, expected) in table.into_iter() {
        if opcode == 0xCB {
            continue;
        }
        let actual = decoded.get(&opcode).unwrap();
        assert_eq!(
            actual.operation.to_string(),
            expected.name,
            "name should be equal, opcode={:#02X}",
            opcode
        );
        assert_eq!(
            actual.bytes, expected.bytes,
            "bytes should be equal, opcode={:#02X}",
            opcode
        );
        assert_eq!(
            actual.cycles, expected.cycles,
            "cycles should be equal, opcode={:#02X}",
            opcode
        );
        assert_eq!(
            actual.cycles_taken, expected.cycles_taken,
            "cycles_taken should be equal, opcode={:#02X}",
            opcode
        );
    }
}
