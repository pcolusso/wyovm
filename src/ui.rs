use iced::widget::{
    button, column, container, keyed, keyed_column, row, scrollable, text, Column, Row,
};
use iced::{alignment, Length};
use num_traits::FromPrimitive;

use crate::{vm, Machine};

type MyMachine = Machine<std::io::Stdin, std::io::Stdout>;

pub struct State {
    pub machine: MyMachine,
    pub running: bool,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Message {
    Step,
    Run,
}

impl State {
    pub fn new(machine: MyMachine) -> Self {
        let running = false;
        Self { machine, running }
    }

    fn mem_row(&self) -> keyed::Column<usize, Message> {
        let range = 0x3000..0x5000;

        keyed_column(range.map(|i| {
            let addr = text(format!("0x{:04x}", i))
                .style(text::success)
                .width(Length::Fill);
            let value = text(format!("0x{:04x}", self.machine.mem[i])).width(Length::Fill);
            (i, row!(addr, value).width(Length::Fill).into())
        }))
        .into()
    }

    fn registers(&self) -> Column<Message> {
        let regs1 = keyed_column((0..5).map(|i| {
            let reg = vm::Register::from_usize(i).unwrap();
            let addr = text(format!("{:?}", reg)).style(text::success);
            let value = text(format!("= 0x{:04x}", self.machine[reg]));
            (i, row!(addr, value).spacing(10.0).into())
        }))
        .width(Length::Fill);
        let regs2 = keyed_column((5..10).map(|i| {
            let reg = vm::Register::from_usize(i).unwrap();
            let addr = text(format!("{:?}", reg)).style(text::success);
            let value = text(format!("= 0x{:04x}", self.machine[reg]));
            (i, row!(addr, value).spacing(10.0).into())
        }))
        .width(Length::Fill);

        column![
            text("Registers").size(32.0),
            container(row!(regs1, regs2))
                .style(container::bordered_box)
                .width(Length::Fill)
                .padding(10.0)
        ]
        .height(Length::Fill)
    }

    fn controls(&self) -> Column<Message> {
        let step = button("Step").on_press(Message::Step);
        let run = button("Run").on_press(Message::Run);

        column![
            text("Controls").size(32.0),
            container(row!(step, run).spacing(10.0))
                .style(container::bordered_box)
                .width(Length::Fill)
                .padding(10.0)
        ]
        .height(Length::Fill)
    }

    pub fn view(&self) -> Row<Message> {
        let padding = 15.0;

        let lhs = column![
            text!("Memory").size(32.0),
            container(scrollable(self.mem_row()).width(Length::Fill))
                .style(container::bordered_box)
                .height(Length::Fill)
                .width(Length::Fill)
                .padding(padding)
        ]
        .height(Length::Fill)
        .width(Length::Fill)
        .padding(padding);
        let rhs = column![self.registers(), self.controls()]
            .height(Length::Fill)
            .width(Length::Fill)
            .align_x(alignment::Alignment::Center)
            .padding(padding);
        row![lhs, rhs].width(Length::Fill).padding(padding)
    }
    pub fn update(&mut self, message: Message) {
        match message {
            Message::Run => {}
            Message::Step => self.machine.cycle(),
        }
    }
}
