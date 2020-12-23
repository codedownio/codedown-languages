x = pkg("list");

printf("[");
for i=1:numel(x)
    printf('{"name": "%s", "version": "%s"}', x(1, i){1}.name, x(1,i){1}.version);

    if i < numel(x)
        printf(",")
    end
end

printf("]\n")
