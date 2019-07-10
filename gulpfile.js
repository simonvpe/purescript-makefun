const gulp = require('gulp');
const purescript = require('gulp-purescript');
const clean = require('gulp-clean');
const run = require('gulp-run');

const sources = ["src/**/*.purs", ".psc-package/*/*/v*/src/**/*.purs"]

gulp.task("make", () => {
    return purescript.compile({
        src: sources
    });
});

gulp.task("run", gulp.series("make", () => {
    const args = process.argv.slice(3).join(" ");
    return run("node index.js " + args)
        .exec();
}));

gulp.task("build", gulp.series("make", () => {
    return run("./node_modules/.bin/pkg --targets node12 -d -o funmake .")
        .exec();
}));

gulp.task("docs", () => {
    return purescript.docs({
        src: sources,
        format: "markdown"
    });
});

gulp.task("dotpsci", () => {
    return purescript.psci({ src: sources })
        .pipe(gulp.dest("."));
});

gulp.task("clean", () => {
    return gulp.src("output", {read: false})
        .pipe(clean());
});

gulp.task("default", gulp.parallel("make", "docs", "dotpsci"));
