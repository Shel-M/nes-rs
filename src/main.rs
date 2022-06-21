mod hardware;

use rand::Rng;
use sdl2::event::Event;
use sdl2::EventPump;
use sdl2::keyboard::Keycode;
use sdl2::pixels::{Color, PixelFormatEnum};
use crate::hardware::cpu::CPU;

fn main() {
    let sdl_context = sdl2::init().unwrap();
    let video_subsystem = sdl_context.video().unwrap();
    let window = video_subsystem
        .window("Snake", (32.0 * 10.0) as u32, (32.0 * 10.0) as u32)
        .position_centered()
        .build().unwrap();

    let mut canvas = window.into_canvas().build().unwrap();
    let mut event_pump = sdl_context.event_pump().unwrap();
    canvas.set_scale(10.0, 10.0).unwrap();

    let creator = canvas.texture_creator();
    let mut texture = creator
        .create_texture_target(PixelFormatEnum::RGB24, 32, 32).unwrap();

    let game_code = vec![
      //0600  0601  0602  0603  0604  0605  0606  0607  0608  0609  060a  060b  060c  060d  060e  060f
        0x20, 0x06, 0x06, 0x20, 0x38, 0x06, 0x20, 0x0d, 0x06, 0x20, 0x2a, 0x06, 0x60, 0xa9, 0x02, 0x85,
      //0610  0611  0612  0613  0614  0615  0616  0617  0618  0619  061a  061b  061c  061d  061e  061f
        0x02, 0xa9, 0x04, 0x85, 0x03, 0xa9, 0x11, 0x85, 0x10, 0xa9, 0x10, 0x85, 0x12, 0xa9, 0x0f, 0x85,
      //0620  0621  0622  0623  0624  0625  0626  0627  0628  0629  062a  062b  062c  062d  062e  062f
        0x14, 0xa9, 0x04, 0x85, 0x11, 0x85, 0x13, 0x85, 0x15, 0x60, 0xa5, 0xfe, 0x85, 0x00, 0xa5, 0xfe,
      //0630  0631  0632  0633  0634  0635  0636  0637  0638  0639  063a  063b  063c  063d  063e  063f
        0x29, 0x03, 0x18, 0x69, 0x02, 0x85, 0x01, 0x60, 0x20, 0x4d, 0x06, 0x20, 0x8d, 0x06, 0x20, 0xc3,
      //0640  0641  0642  0643  0644  0645  0646  0647  0648  0649  064a  064b  064c  064d  064e  064f
        0x06, 0x20, 0x19, 0x07, 0x20, 0x20, 0x07, 0x20, 0x2d, 0x07, 0x4c, 0x38, 0x06, 0xa5, 0xff, 0xc9,
      //0650  0651  0652  0653  0654  0655  0656  0657  0658  0659  065a  065b  065c  065d  065e  065f
        0x77, 0xf0, 0x0d, 0xc9, 0x64, 0xf0, 0x14, 0xc9, 0x73, 0xf0, 0x1b, 0xc9, 0x61, 0xf0, 0x22, 0x60,
      //0660  0661  0662  0663  0664  0665  0666  0667  0668  0669  066a  066b  066c  066d  066e  066f
        0xa9, 0x04, 0x24, 0x02, 0xd0, 0x26, 0xa9, 0x01, 0x85, 0x02, 0x60, 0xa9, 0x08, 0x24, 0x02, 0xd0,
      //0670  0671  0672  0673  0674  0675  0676  0677  0678  0679  067a  067b  067c  067d  067e  067f
        0x1b, 0xa9, 0x02, 0x85, 0x02, 0x60, 0xa9, 0x01, 0x24, 0x02, 0xd0, 0x10, 0xa9, 0x04, 0x85, 0x02,
      //0680  0681  0682  0683  0684  0685  0686  0687  0688  0689  068a  068b  068c  068d  068e  068f
        0x60, 0xa9, 0x02, 0x24, 0x02, 0xd0, 0x05, 0xa9, 0x08, 0x85, 0x02, 0x60, 0x60, 0x20, 0x94, 0x06,
      //0690  0691  0692  0693  0694  0695  0696  0697  0698  0699  069a  069b  069c  069d  069e  069f
        0x20, 0xa8, 0x06, 0x60, 0xa5, 0x00, 0xc5, 0x10, 0xd0, 0x0d, 0xa5, 0x01, 0xc5, 0x11, 0xd0, 0x07,
      //06a0  06a1  06a2  06a3  06a4  06a5  06a6  06a7  06a8  06a9  06aa  06ab  06ac  06ad  06ae  06af
        0xe6, 0x03, 0xe6, 0x03, 0x20, 0x2a, 0x06, 0x60, 0xa2, 0x02, 0xb5, 0x10, 0xc5, 0x10, 0xd0, 0x06,
      //06b0  06b1  06b2  06b3  06b4  06b5  06b6  06b7  06b8  06b9  06ba  06bb  06bc  06bd  06be  06bf
        0xb5, 0x11, 0xc5, 0x11, 0xf0, 0x09, 0xe8, 0xe8, 0xe4, 0x03, 0xf0, 0x06, 0x4c, 0xaa, 0x06, 0x4c,
      //06c0  06c1  06c2  06c3  06c4  06c5  06c6  06c7  06c8  06c9  06ca  06cb  06cc  06cd  06ce  06cf
        0x35, 0x07, 0x60, 0xa6, 0x03, 0xca, 0x8a, 0xb5, 0x10, 0x95, 0x12, 0xca, 0x10, 0xf9, 0xa5, 0x02,
      //06d0  06d1  06d2  06d3  06d4  06d5  06d6  06d7  06d8  06d9  06da  06db  06dc  06dd  06de  06df
        0x4a, 0xb0, 0x09, 0x4a, 0xb0, 0x19, 0x4a, 0xb0, 0x1f, 0x4a, 0xb0, 0x2f, 0xa5, 0x10, 0x38, 0xe9,
      //06e0  06e1  06e2  06e3  06e4  06e5  06e6  06e7  06e8  06e9  06ea  06eb  06ec  06ed  06ee  06ef
        0x20, 0x85, 0x10, 0x90, 0x01, 0x60, 0xc6, 0x11, 0xa9, 0x01, 0xc5, 0x11, 0xf0, 0x28, 0x60, 0xe6,
      //06f0  06f1  06f2  06f3  06f4  06f5  06f6  06f7  06f8  06f9  06fa  06fb  06fc  06fd  06fe  06ff
        0x10, 0xa9, 0x1f, 0x24, 0x10, 0xf0, 0x1f, 0x60, 0xa5, 0x10, 0x18, 0x69, 0x20, 0x85, 0x10, 0xb0,
      //0700  0701  0702  0703  0704  0705  0706  0707  0708  0709  070a  070b  070c  070d  070e  070f
        0x01, 0x60, 0xe6, 0x11, 0xa9, 0x06, 0xc5, 0x11, 0xf0, 0x0c, 0x60, 0xc6, 0x10, 0xa5, 0x10, 0x29,
      //0710  0711  0712  0713  0714  0715  0716  0717  0718  0719  071a  071b  071c  071d  071e  071f
        0x1f, 0xc9, 0x1f, 0xf0, 0x01, 0x60, 0x4c, 0x35, 0x07, 0xa0, 0x00, 0xa5, 0xfe, 0x91, 0x00, 0x60,
      //0720  0721  0722  0723  0724  0725  0726  0727  0728  0729  072a  072b  072c  072d  072e  072f
        0xa6, 0x03, 0xa9, 0x00, 0x81, 0x10, 0xa2, 0x00, 0xa9, 0x01, 0x81, 0x10, 0x60, 0xa2, 0x00, 0xea,
      //0730  0731  0732  0733  0734  0735  0736  0737  0738  0739  073a  073b  073c  073d  073e  073f
        0xea, 0xca, 0xd0, 0xfb, 0x60
    ];

    let mut cpu = CPU::new();
    cpu.load(game_code);
    cpu.reset();

    let mut screen_state = [0; 32 * 3 * 32];
    let mut rng = rand::thread_rng();

    cpu.run_with_callback(move |cpu| {
        handle_user_input(cpu, &mut event_pump);
        //cpu.mem_write(0xfe, rng.gen_range(1, 16));

        if read_screen_state(cpu, &mut screen_state) {
            texture.update(None, &screen_state, 32 * 3).unwrap();
            canvas.copy(&texture, None, None).unwrap();
            canvas.present();
        }

        std::thread::sleep(std::time::Duration::new(0, 10_000));
    })
}

fn read_screen_state(cpu: &CPU, frame: &mut [u8; 32 * 3 * 32]) -> bool {
    let mut frame_idx = 0;
    let mut update = false;

    /*for i in 0x0200..0x0600 {
        let color_idx = cpu.mem_read(i as u16);
        let (b1, b2, b3)= color(color_idx).rgb();
        if frame[frame_idx] != b1 || frame[frame_idx + 1] != b2 || frame[frame_idx + 2] != b3 {
            frame[frame_idx] = b1;
            frame[frame_idx + 1] = b2;
            frame[frame_idx + 2] = b3;
            update = true
        }
        frame_idx += 3;
    }*/

    update
}

fn handle_user_input(cpu: &mut CPU, event_pump: &mut EventPump) {
    /*for event in event_pump.poll_iter() {
        match event {
            Event::Quit { .. } | Event::KeyDown { keycode: Some(Keycode::Escape), .. } => {
                std::process::exit(0);
            },
            Event::KeyDown { keycode: Some(Keycode::W), .. } => {
                cpu.mem_write(0xff, 0x77);
            },
            Event::KeyDown { keycode: Some(Keycode::S), .. } => {
                cpu.mem_write(0xff, 0x73);
            },
            Event::KeyDown { keycode: Some(Keycode::A), .. } => {
                cpu.mem_write(0xff, 0x61);
            },
            Event::KeyDown { keycode: Some(Keycode::D), .. } => {
                cpu.mem_write(0xff, 0x64);
            },
            _ => { }
        }
    }*/
}

fn color(byte: u8) -> Color {
    match byte {
        0 => Color::BLACK,
        1 => Color::WHITE,
        2 | 9 => Color::GREY,
        3 | 10 => Color::RED,
        4 | 11 => Color::GREEN,
        5 | 12 => Color::BLUE,
        6 | 13 => Color::MAGENTA,
        7 | 14 => Color::YELLOW,
        _ => Color::CYAN,
    }
}
