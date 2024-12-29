use iced::overlay::Element;
use iced::widget::shader::wgpu::naga::proc::Alignment;
use iced::widget::{column, container, keyed, keyed_column, row, scrollable, text, Column, Row};
use iced::{alignment, Length};

use crate::Machine;

pub struct State {
    pub machine: Machine,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Message {
    Step,
    Run,
    Restart,
}

impl State {
    fn mem_row(&self) -> keyed::Column<usize, Message> {
        let range = 0x2000..0x5000;

        keyed_column(range.map(|i| {
            let addr = text(format!("0x{:0x}", i)).style(text::success);
            let value =
                text(format!("0x{:0x}", self.machine.mem[i])).align_x(alignment::Alignment::Start);
            (i, row!(addr, value).into())
        }))
        .into()
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
        let rhs = column!["second column"]
            .height(Length::Fill)
            .width(Length::Fill)
            .align_x(alignment::Alignment::Center)
            .padding(padding);
        row![lhs, rhs].width(Length::Fill).padding(padding)
    }
    pub fn update(&mut self, message: Message) {
        todo!()
    }
}
