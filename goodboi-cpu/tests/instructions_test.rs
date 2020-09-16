use serde::Deserialize;
use std::collections::{BTreeMap, HashMap};

use goodboi_cpu::instructions::{self, Instruction};

const INSTRUCTION_TABLES_JSON: &str = std::include_str!("data/instructions.json");

#[derive(Debug, Deserialize, PartialEq)]
struct InstructionData {
    name: String,
    bytes: u32,
    cycles: u32,
    cycles_taken: Option<u32>,
}

impl From<Instruction> for InstructionData {
    fn from(instruction: Instruction) -> Self {
        Self {
            name: instruction.operation.to_string(),
            bytes: instruction.bytes,
            cycles: instruction.cycles,
            cycles_taken: instruction.cycles_taken,
        }
    }
}

type InstructionTable = BTreeMap<u8, InstructionData>;

fn load_tables() -> HashMap<String, InstructionTable> {
    let tables: HashMap<String, BTreeMap<String, InstructionData>> =
        serde_json::from_str(INSTRUCTION_TABLES_JSON).unwrap();
    tables
        .into_iter()
        .map(|(key, table)| {
            (
                key,
                table
                    .into_iter()
                    .map(|(opcode, data)| (u8::from_str_radix(&opcode, 16).unwrap(), data))
                    .collect(),
            )
        })
        .collect()
}

fn test_instructions_decoded_correctly(table: &InstructionTable, prefix: Option<u8>) {
    let decoded: BTreeMap<_, _> = (0..=255)
        .filter_map(|opcode| {
            let mut bytes = prefix.map(|prefix| vec![prefix]).unwrap_or_else(Vec::new);
            bytes.push(opcode);
            bytes.push(0x34);
            bytes.push(0x12);
            Some((
                opcode,
                instructions::decode(&mut bytes.into_iter()).unwrap(),
            ))
        })
        .collect();

    for (opcode, expected) in table.iter() {
        let actual = decoded.get(&opcode);
        assert!(actual.is_some(), "opcode {:#04X} should be decoded", opcode);
        let actual: InstructionData = (*actual.unwrap()).into();
        assert_eq!(
            actual, *expected,
            "opcode {:#04X} should be decoded to correct instruction",
            opcode,
        );
    }
}

#[test]
fn all_unprefixed_instructions_are_decoded_correctly() {
    let tables = load_tables();

    let unprefixed = tables.get("unprefixed").unwrap();
    let cbprefixed = tables.get("cbprefixed").unwrap();

    test_instructions_decoded_correctly(unprefixed, None);
    test_instructions_decoded_correctly(cbprefixed, Some(0xCB));
}
