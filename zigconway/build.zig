const Builder = @import("std").build.Builder;
const builtin = @import("builtin");

pub fn build(b: *Builder) void {
    const mode = b.standardReleaseOptions();

    // var t = b.addTest("test.zig");
    // t.linkSystemLibrary("c");
    // const test_step = b.step("test", "Run all tests");
    // test_step.dependOn(&t.step);

    var exe = b.addExecutable("conway", "src/main.zig");
    exe.setBuildMode(mode);
    
    b.installArtifact(exe);

    b.default_step.dependOn(&exe.step);

}
